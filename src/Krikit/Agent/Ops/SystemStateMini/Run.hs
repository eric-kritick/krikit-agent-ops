{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | @krikit-regen-system-state-mini@: regenerate the human-readable
-- mirror of the canonical mini infrastructure config.
--
-- Canonical source:
-- @kritick-ecosystem\/config\/infrastructure-macmini.json@.
-- Output: @krikit-agent-fabric\/context\/system-state-mini.generated.md@.
--
-- The output isn\'t a 1:1 dump of the JSON. It\'s the agent-facing
-- view -- LLM channel summary, deployed services, ops binaries,
-- security posture, regeneration provenance. Anything an agent
-- needs to know without parsing JSON.
module Krikit.Agent.Ops.SystemStateMini.Run
    ( -- * Domain types
      Config (..)
    , LLMChannel (..)
    , Service (..)
    , HardeningItem (..)

      -- * IO
    , readConfig

      -- * Pure rendering
    , renderReport
    ) where

import           Data.Aeson                       (FromJSON (..), Value, (.:), (.:?))
import qualified Data.Aeson                       as A
import qualified Data.Aeson.Key                   as Key
import qualified Data.Aeson.KeyMap                as KM
import qualified Data.Aeson.Types                 as AT
import qualified Data.ByteString.Lazy             as LBS
import           Data.List                        (sortOn)
import           Data.Maybe                       (fromMaybe)
import           Data.Text                        (Text)
import qualified Data.Text                        as T

import           Krikit.Agent.Ops.Regen.Banner
    ( Banner (..)
    , BannerInputs (..)
    , renderBanner
    )

-- =============================================================================
-- Domain types
-- =============================================================================

-- | Top-level config we care about. Many fields in the JSON are
-- irrelevant to the agent-facing view (network phases, knowledge
-- base IDs, etc.); we only decode what gets rendered.
data Config = Config
    { cfgHostName    :: !Text
    , cfgLLMChannels :: ![(Text, LLMChannel)]
      -- ^ key = channel id (sentry, workhorse, ...) preserving JSON order
    , cfgServices    :: ![(Text, Service)]
    , cfgHardening   :: ![(Text, HardeningItem)]
    }
    deriving stock (Eq, Show)

-- | One LLM channel as the agent should see it.
data LLMChannel = LLMChannel
    { lcProvider     :: !Text
    , lcPrimaryModel :: !(Maybe Text)
    , lcEffort       :: !(Maybe Text)
    , lcCost         :: !(Maybe Text)
    , lcAuth         :: !(Maybe Text)
    , lcUseCases     :: ![Text]
    }
    deriving stock (Eq, Show)

-- | One launchd-managed service.
data Service = Service
    { svUser   :: !(Maybe Text)
    , svType   :: !(Maybe Text)
    , svLabel  :: !(Maybe Text)
    , svStatus :: !(Maybe Text)
    }
    deriving stock (Eq, Show)

-- | One hardening checklist row.
data HardeningItem = HardeningItem
    { hiTarget   :: !Text   -- ^ rendered (true/false/string)
    , hiVerified :: !Text   -- ^ rendered (true/false)
    }
    deriving stock (Eq, Show)

-- =============================================================================
-- JSON decoding
-- =============================================================================

-- The aeson decoders deliberately keep the @Value@ map's *insertion*
-- order in the rendered output. KeyMap is unordered, so for
-- deterministic markdown we sort keys alphabetically except where a
-- known canonical ordering applies (see 'channelOrder').
instance FromJSON Config where
    parseJSON = A.withObject "Config" $ \o -> do
        host       <- o .: "host"
        hostName   <- host .: "name"

        -- LLM channels: order matters (sentry < workhorse < thinker <
        -- builder < overflow < carousel). The JSON object is ordered
        -- this way already; we use 'channelOrder' to enforce the
        -- output order even if a future edit shuffles the keys.
        llmObj     <- o .: "llm_channels"
        llmPairs   <- mapKeyMap parseChannel llmObj

        servicesObj <- o .: "services"
        servicePairs <- mapKeyMap parseService servicesObj

        hardeningObj <- o .: "hardening"
        hardeningPairs <- mapKeyMap parseHardening hardeningObj

        pure Config
            { cfgHostName    = hostName
            , cfgLLMChannels = sortOnKey channelOrder llmPairs
            , cfgServices    = sortOn fst servicePairs
            , cfgHardening   = sortOn fst hardeningPairs
            }

parseChannel :: Value -> AT.Parser LLMChannel
parseChannel = A.withObject "LLMChannel" $ \o -> do
    provider <- o .: "provider"
    pm       <- o .:? "primary_model"
    effort   <- o .:? "thinking_default"
    cost     <- (o .:? "cost") <|>? (fmap renderCostMap <$> (o .:? "cost_per_m_tokens"))
    auth     <- o .:? "auth"
    useCases <- fromMaybe [] <$> o .:? "use_cases"
    pure LLMChannel
        { lcProvider     = provider
        , lcPrimaryModel = pm
        , lcEffort       = effort
        , lcCost         = cost
        , lcAuth         = auth
        , lcUseCases     = useCases
        }
  where
    -- '<|>' for parsers that already wrap Maybe.
    (<|>?) :: AT.Parser (Maybe a) -> AT.Parser (Maybe a) -> AT.Parser (Maybe a)
    p <|>? q = do
        r <- p
        case r of
            Just _  -> pure r
            Nothing -> q

parseService :: Value -> AT.Parser Service
parseService = A.withObject "Service" $ \o ->
    Service
        <$> o .:? "user"
        <*> o .:? "type"
        <*> o .:? "label"
        <*> o .:? "status"

parseHardening :: Value -> AT.Parser HardeningItem
parseHardening = A.withObject "HardeningItem" $ \o -> do
    target   <- o .: "target"
    verified <- o .:? "verified"
    pure HardeningItem
        { hiTarget   = renderJsonScalar target
        , hiVerified = case verified of
            Nothing -> "?"
            Just v  -> renderJsonScalar v
        }

mapKeyMap :: (Value -> AT.Parser a) -> Value -> AT.Parser [(Text, a)]
mapKeyMap f v = case v of
    A.Object km ->
        traverse (\(k, val) -> (Key.toText k,) <$> f val) (KM.toList km)
    _ -> AT.typeMismatch "Object" v

-- | Render a JSON scalar (Bool / Number / String / Null) to a short
-- text suitable for a markdown cell. We don\'t expect arrays or
-- objects here.
renderJsonScalar :: Value -> Text
renderJsonScalar = \case
    A.String s     -> s
    A.Bool True    -> "yes"
    A.Bool False   -> "no"
    A.Number n     -> T.pack (show n)
    A.Null         -> "—"
    other          -> T.pack (show other)

renderCostMap :: Value -> Text
renderCostMap = \case
    A.Object _ -> "metered (see config)"
    other      -> renderJsonScalar other

-- | Canonical channel display order, regardless of JSON key order.
-- Unknown keys sort alphabetically after the known ones.
channelOrder :: Text -> (Int, Text)
channelOrder k = case lookup k known of
    Just i  -> (i, k)
    Nothing -> (length known, k)
  where
    known =
        [ ("sentry"   , 0)
        , ("workhorse", 1)
        , ("thinker"  , 2)
        , ("builder"  , 3)
        , ("overflow" , 4)
        , ("carousel" , 5)
        ]

sortOnKey :: Ord b => (a -> b) -> [(a, c)] -> [(a, c)]
sortOnKey f = sortOn (f . fst)

-- | Read and parse the canonical mini infrastructure JSON. Errors
-- are surfaced as @Text@ rather than throwing -- the caller
-- (executable @Main@) is responsible for exit codes.
readConfig :: FilePath -> IO (Either Text Config)
readConfig path = do
    bytes <- LBS.readFile path
    case A.eitherDecode bytes of
        Left err  -> pure (Left ("could not parse " <> T.pack path <> ": " <> T.pack err))
        Right cfg -> pure (Right cfg)

-- =============================================================================
-- Rendering
-- =============================================================================

-- | Render the full markdown report. Pure: same input, same output.
-- 'banLastGenerated' is the only date-like value; the writer
-- normalizes that line away during idempotency comparison.
renderReport
    :: Text     -- ^ ISO date for the @Last generated@ banner line
    -> Config
    -> Text
renderReport isoDate cfg =
    T.unlines
        [ "# System state — mini"
        , ""
        , renderBanner banner
        , ""
        , "Snapshot of current operational configuration on **"
            <> cfgHostName cfg
            <> "** (the kritick agent host). For broader kritick"
            <> " infrastructure (AWS topology, MongoDB Atlas cluster,"
            <> " OpenSearch domain, ElastiCache, GitHub repos), see"
            <> " `system-state-kritick.generated.md`."
        , ""
        , renderLLMChannels (cfgLLMChannels cfg)
        , ""
        , effortWiringSection
        , ""
        , renderServices (cfgServices cfg)
        , ""
        , renderHardening (cfgHardening cfg)
        , ""
        , gapsSection
        ]
  where
    banner = Banner
        { banGeneratorName = "krikit-regen-system-state-mini"
        , banInputs        =
            [ BannerInputs "`kritick-ecosystem/config/infrastructure-macmini.json` (canonical JSON)"
            , BannerInputs "live `launchctl print` and `which krikit-*` (planned, post Connections RFC Phase 1)"
            ]
        , banTrigger       = "on `infrastructure-macmini.json` change + cron 06:30 daily fallback"
        , banLastGenerated = isoDate
        }

renderLLMChannels :: [(Text, LLMChannel)] -> Text
renderLLMChannels chans =
    T.unlines $
        [ "## LLM channels"
        , ""
        , "| Channel | Provider/model | Effort | Auth | Cost |"
        , "|---|---|---|---|---|"
        ]
        ++ map row chans
  where
    row (name, ch) = T.intercalate " | "
        [ "| " <> name
        , fromMaybe "—" (lcPrimaryModel ch)
        , maybe "—" (("`" <>) . (<> "`")) (lcEffort ch)
        , fromMaybe "—" (lcAuth ch)
        , fromMaybe "—" (lcCost ch) <> " |"
        ]

effortWiringSection :: Text
effortWiringSection = T.unlines
    [ "## Reasoning-effort wiring"
    , ""
    , "- Per-agent `thinkingDefault` in `openclaw.json` (auto-reply path)."
    , "- Per-model `agents.defaults.models[ref].params.thinking` in"
    , "  `openclaw.json` (CLI agent command path)."
    , "- Per-workspace `.claude-effort` files +"
    , "  `~agentops/.local/bin/claude-effort-wrapper.sh` (wire-level"
    , "  `--effort` for claude-cli routes)."
    , "- Source: PB 4 §3.11 + §3.12."
    ]

renderServices :: [(Text, Service)] -> Text
renderServices svcs =
    T.unlines $
        [ "## Services (launchd)"
        , ""
        , "| Key | Label | User | Type | Status |"
        , "|---|---|---|---|---|"
        ]
        ++ map row svcs
  where
    row (name, s) = T.intercalate " | "
        [ "| " <> name
        , fromMaybe "—" (svLabel s)
        , fromMaybe "—" (svUser s)
        , fromMaybe "—" (svType s)
        , fromMaybe "—" (svStatus s) <> " |"
        ]

renderHardening :: [(Text, HardeningItem)] -> Text
renderHardening items =
    T.unlines $
        [ "## Security posture (hardening)"
        , ""
        , "| Item | Target | Verified |"
        , "|---|---|---|"
        ]
        ++ map row items
  where
    row (name, h) = T.intercalate " | "
        [ "| " <> name
        , hiTarget h
        , hiVerified h <> " |"
        ]

gapsSection :: Text
gapsSection = T.unlines
    [ "## Known gaps / pending work"
    , ""
    , "See `current-priorities.md`."
    ]
