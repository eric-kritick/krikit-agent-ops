{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | @krikit-verify-llm-channel-consistency@: cross-check the
-- canonical LLM-channel declarations in
-- @kritick-ecosystem\/config\/infrastructure-macmini.json@ against
-- what OpenClaw actually has configured in
-- @\/Users\/agentops\/.openclaw\/openclaw.json@.
--
-- V1 scope: channels that declare @openclaw_agent_id@. Today
-- that's @thinker@ and @builder@ (the claude-cli agents). For
-- each such channel, verify the matching OpenClaw agent's
-- @model@ equals the channel's @primary_model@ and the agent's
-- @thinkingDefault@ equals the channel's @thinking_default@.
--
-- The verifier degrades gracefully when openclaw.json is not on
-- disk (typical off-mini run): one 'Warning' per channel that
-- would have been checked; no 'Error' findings. On the mini both
-- files are present, so mismatches surface as 'Error'.
--
-- Future scope (V2):
--
--   * Per-agent @.claude-effort@ files: their content should
--     equal the channel's @cli_effort@.
--   * @claude-effort-wrapper.sh@: presence + executable bit.
--   * Channels without @openclaw_agent_id@ (sentry, workhorse)
--     -- match by name convention or extend the JSON to make
--     the link explicit.
module Krikit.Agent.Ops.Verify.LlmChannelConsistency
    ( -- * Domain types
      Channel (..)
    , Agent (..)

      -- * Pure logic
    , buildFindings

      -- * Effectful orchestrator
    , verify
    ) where

import           Control.Exception                        (IOException, try)
import           Data.Aeson                               (FromJSON (..),
                                                           (.:), (.:?))
import qualified Data.Aeson                               as A
import qualified Data.Aeson.Key                           as Key
import qualified Data.Aeson.KeyMap                        as KM
import qualified Data.ByteString.Lazy                     as LBS
import           Data.List                                (sortOn)
import           Data.Map.Strict                          (Map)
import qualified Data.Map.Strict                          as Map
import           Data.Maybe                               (mapMaybe)
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import           System.Directory                         (doesFileExist)

import           Effectful                                (Eff, IOE, liftIO, (:>))
import           Effectful.Reader.Static                  (Reader, ask)

import           Krikit.Agent.Ops.Config
    ( Config (..)
    , EcosystemPaths (..)
    , OpenclawPaths (..)
    , PathsConfig (..)
    )
import           Krikit.Agent.Ops.Verify.Common           (Finding (..),
                                                           Severity (..))

-- =============================================================================
-- Domain types
-- =============================================================================

-- | One LLM channel declaration extracted from
-- @infrastructure-macmini.json@'s @llm_channels@ map.
data Channel = Channel
    { chName            :: !Text
      -- ^ map key, e.g. @\"thinker\"@.
    , chPrimaryModel    :: !(Maybe Text)
      -- ^ @primary_model@; absent for some channels (overflow,
      -- carousel) where we don't pin a single model.
    , chThinkingDefault :: !(Maybe Text)
    , chOpenclawAgentId :: !(Maybe Text)
      -- ^ @openclaw_agent_id@ -- the explicit linkage to an
      -- agent in @openclaw.json@. Only channels with this set
      -- are cross-checked in V1.
    }
    deriving stock (Eq, Show)

-- | One OpenClaw agent extracted from @openclaw.json@'s
-- @agents.list[]@.
data Agent = Agent
    { agId              :: !Text
    , agModel           :: !(Maybe Text)
    , agThinkingDefault :: !(Maybe Text)
    }
    deriving stock (Eq, Show)

-- =============================================================================
-- JSON decoding
-- =============================================================================

instance FromJSON Channel where
    parseJSON = A.withObject "Channel" $ \o ->
        Channel
            <$> pure ""              -- caller fills in the map key
            <*> o .:? "primary_model"
            <*> o .:? "thinking_default"
            <*> o .:? "openclaw_agent_id"

instance FromJSON Agent where
    parseJSON = A.withObject "Agent" $ \o ->
        Agent
            <$> o .:  "id"
            <*> o .:? "model"
            <*> o .:? "thinkingDefault"

-- | Decode the @llm_channels@ block of @infrastructure-macmini.json@.
-- The top-level JSON has many other fields we don't care about.
decodeChannels :: LBS.ByteString -> Either Text [Channel]
decodeChannels bytes = case A.eitherDecode bytes of
    Left err -> Left ("infrastructure-macmini.json parse: " <> T.pack err)
    Right (A.Object outer) -> case KM.lookup "llm_channels" outer of
        Just (A.Object km) -> Right (mapMaybe pair (KM.toList km))
          where
            pair (k, v) = case A.fromJSON v of
                A.Success ch -> Just (ch { chName = Key.toText k })
                A.Error   _  -> Nothing
        _ -> Left "infrastructure-macmini.json: missing or non-object llm_channels"
    Right _  -> Left "infrastructure-macmini.json: top-level is not an object"

-- | Decode the @agents.list[]@ block of @openclaw.json@.
decodeAgents :: LBS.ByteString -> Either Text [Agent]
decodeAgents bytes = case A.eitherDecode bytes of
    Left err -> Left ("openclaw.json parse: " <> T.pack err)
    Right (A.Object outer) -> case KM.lookup "agents" outer of
        Just (A.Object agentsObj) -> case KM.lookup "list" agentsObj of
            Just (A.Array arr) -> case traverse parseEither (foldr (:) [] arr) of
                Right xs -> Right xs
                Left  e  -> Left ("openclaw.json agents.list[] item: " <> T.pack e)
            _ -> Left "openclaw.json: agents.list is missing or not an array"
        _ -> Left "openclaw.json: missing or non-object agents"
    Right _  -> Left "openclaw.json: top-level is not an object"
  where
    parseEither v = case A.fromJSON v of
        A.Success a -> Right a
        A.Error   e -> Left e

-- =============================================================================
-- Pure cross-check
-- =============================================================================

-- | Pure core: given the parsed channels and agents, return all
-- findings.
--
-- Iterates channels with @openclaw_agent_id@ set; for each one,
-- looks up the agent by id and emits per-mismatch findings. A
-- channel whose openclaw_agent_id has no matching agent is itself
-- a finding.
buildFindings :: [Channel] -> [Agent] -> [Finding]
buildFindings channels agents =
    concatMap checkOne (sortOn chName channels)
  where
    agentMap :: Map Text Agent
    agentMap = Map.fromList [(agId a, a) | a <- agents]

    checkOne :: Channel -> [Finding]
    checkOne ch = case chOpenclawAgentId ch of
        Nothing      -> []                            -- not in V1 scope
        Just agentId -> case Map.lookup agentId agentMap of
            Nothing -> [missingAgent ch agentId]
            Just a  -> compareModel ch a ++ compareThinking ch a

    missingAgent :: Channel -> Text -> Finding
    missingAgent ch agentId = Finding
        { fSeverity = Error
        , fSubject  = chName ch
        , fMessage  = "channel declares openclaw_agent_id `"
                   <> agentId
                   <> "` but openclaw.json has no agent with that id"
        }

    compareModel :: Channel -> Agent -> [Finding]
    compareModel ch a = case (chPrimaryModel ch, agModel a) of
        (Just inf, Just oc) | inf /= oc ->
            [ Finding Error (chName ch)
                ("model mismatch: infrastructure says `" <> inf
                <> "`, openclaw says `" <> oc <> "`")
            ]
        (Just _, Nothing) ->
            [ Finding Error (chName ch)
                "channel has primary_model but openclaw agent has no model field"
            ]
        _ -> []

    compareThinking :: Channel -> Agent -> [Finding]
    compareThinking ch a = case (chThinkingDefault ch, agThinkingDefault a) of
        (Just inf, Just oc) | inf /= oc ->
            [ Finding Error (chName ch)
                ("thinking_default mismatch: infrastructure says `"
                <> inf <> "`, openclaw says `" <> oc <> "`")
            ]
        _ -> []

-- =============================================================================
-- Effectful orchestrator
-- =============================================================================

-- | End-to-end verifier. Reads both source files; gracefully
-- degrades when openclaw.json is missing (off-mini case) by
-- emitting one 'Warning' per channel that would have been
-- cross-checked plus one 'Warning' explaining the source absence.
verify
    :: ( Reader Config :> es
       , IOE :> es
       )
    => Eff es (Either Text [Finding])
verify = do
    cfg <- ask
    let infraPath    = epInfrastructureMacminiJson . pcEcosystem . cfgPaths $ cfg
        openclawPath = opConfigJson                . pcOpenclaw  . cfgPaths $ cfg

    infraBytesE <- liftIO (try (LBS.readFile infraPath))
    case infraBytesE of
        Left (e :: IOException) ->
            pure (Left ("could not read " <> T.pack infraPath
                        <> ": " <> T.pack (show e)))
        Right infraBytes -> case decodeChannels infraBytes of
            Left err       -> pure (Left err)
            Right channels -> do
                ocExists <- liftIO (doesFileExist openclawPath)
                if not ocExists
                    then pure (Right (degradedFindings openclawPath channels))
                    else do
                        ocBytesE <- liftIO (try (LBS.readFile openclawPath))
                        case ocBytesE of
                            Left (e :: IOException) ->
                                pure (Left ("could not read " <> T.pack openclawPath
                                            <> ": " <> T.pack (show e)))
                            Right ocBytes -> case decodeAgents ocBytes of
                                Left err     -> pure (Left err)
                                Right agents ->
                                    pure (Right (buildFindings channels agents))

-- | Degraded-mode findings: openclaw.json absent. One Warning
-- explaining the absence, plus one Warning per channel whose
-- check we skipped, so the operator sees exactly which subset
-- of the configuration didn't get verified.
degradedFindings :: FilePath -> [Channel] -> [Finding]
degradedFindings openclawPath channels =
    Finding Warning "openclaw.json"
        ("source not found at `" <> T.pack openclawPath
        <> "`; cross-check skipped (this is expected off-mini)")
    : [ Finding Warning (chName ch)
            "skipped: openclaw.json absent"
      | ch <- sortOn chName channels
      , Just _ <- [chOpenclawAgentId ch]
      ]
