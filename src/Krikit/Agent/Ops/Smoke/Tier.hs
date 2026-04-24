{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Tier domain types for the smoke test.
--
-- No IO, no effects — just ADTs + tiny pure helpers. Callers build
-- 'TierResult' values from their per-tier runners; formatters turn the
-- results into output.
module Krikit.Agent.Ops.Smoke.Tier
    ( -- * Tier identity
      Tier (..)
    , tierName
    , tierKey

      -- * Services checked by 'TierServices'
    , Service (..)
    , serviceLaunchctlLabel

      -- * Agents exercised by 'TierSentry' / 'TierWorkhorse' / 'TierThinker' / 'TierBuilder'
    , Agent (..)
    , agentOpenclawName
    , agentDisplayName
    , agentToTier
    , tierToAgent

      -- * Status of a tier run
    , TierStatus (..)
    , isFailure

      -- * Full tier result
    , TierResult (..)
    , pass
    , failWith
    , skipWith

      -- * Aggregate counts across a run
    , Counts (..)
    , countResults
    , allPassed
    ) where

import Data.Text (Text)

import Krikit.Agent.Ops.Units (Milliseconds (..))

-- | All tiers the smoke test knows about. Bounded+Enum so we can iterate.
data Tier
    = TierServices
    | TierDocker
    | TierOllama
    | TierAudit
    | TierSentry
    | TierWorkhorse
    | TierThinker
    | TierBuilder
    | TierConfig
    | TierErrLog
    | TierTelegram
    deriving stock (Eq, Show, Bounded, Enum)

-- | Human-readable display name for a tier (used in banner lines).
tierName :: Tier -> Text
tierName = \case
    TierServices  -> "launchd services"
    TierDocker    -> "Docker / Colima"
    TierOllama    -> "Ollama"
    TierAudit     -> "openclaw security audit"
    TierSentry    -> "Sentry / Wicket (main)"
    TierWorkhorse -> "Workhorse / Montag"
    TierThinker   -> "Thinker / Faber"
    TierBuilder   -> "Builder / Stendahl"
    TierConfig    -> "Config assertions"
    TierErrLog    -> "Sandbox health (err log)"
    TierTelegram  -> "Telegram round-trip"

-- | Machine-friendly key for a tier (stable across renames of display names;
-- use this in the JSON history log).
tierKey :: Tier -> Text
tierKey = \case
    TierServices  -> "services"
    TierDocker    -> "docker"
    TierOllama    -> "ollama"
    TierAudit     -> "audit"
    TierSentry    -> "sentry"
    TierWorkhorse -> "workhorse"
    TierThinker   -> "thinker"
    TierBuilder   -> "builder"
    TierConfig    -> "config"
    TierErrLog    -> "errlog"
    TierTelegram  -> "telegram"

-- === Services ==============================================================

-- | A launchd service checked by 'TierServices'. Carrying this as an ADT
-- instead of raw 'Text' prevents stringly-typed mistakes ("ai.krikit.colima"
-- vs "ai.krikit.Colima") and forces update in every match when we add or
-- rename a service.
data Service
    = ServiceColima
    | ServiceOllama
    | ServiceOpenclaw
    | ServiceMonitor
    | ServiceWorkspaceBackup
    deriving stock (Eq, Show, Bounded, Enum)

-- | The exact label launchctl expects, as in @launchctl print system/<label>@.
serviceLaunchctlLabel :: Service -> Text
serviceLaunchctlLabel = \case
    ServiceColima          -> "ai.krikit.colima"
    ServiceOllama          -> "ai.krikit.ollama"
    ServiceOpenclaw        -> "ai.krikit.openclaw"
    ServiceMonitor         -> "ai.krikit.monitor"
    ServiceWorkspaceBackup -> "ai.krikit.workspace-backup"

-- === Agents ================================================================

-- | One of the four agents the smoke test exercises via the openclaw CLI.
-- ADT rather than raw 'Text' for the same reason as 'Service': prevents
-- typos and forces compiler-checked update on every match site.
data Agent
    = AgentMain       -- ^ Sentry persona (Wicket)
    | AgentWorkhorse  -- ^ Montag
    | AgentThinker    -- ^ Faber
    | AgentBuilder    -- ^ Stendahl
    deriving stock (Eq, Show, Bounded, Enum)

-- | The exact label passed to @openclaw agent --agent <name>@.
agentOpenclawName :: Agent -> Text
agentOpenclawName = \case
    AgentMain      -> "main"
    AgentWorkhorse -> "workhorse"
    AgentThinker   -> "thinker"
    AgentBuilder   -> "builder"

-- | Human-readable label for log lines / summary output.
agentDisplayName :: Agent -> Text
agentDisplayName = \case
    AgentMain      -> "Sentry (Wicket)"
    AgentWorkhorse -> "Workhorse (Montag)"
    AgentThinker   -> "Thinker (Faber)"
    AgentBuilder   -> "Builder (Stendahl)"

-- | Every 'Agent' corresponds to exactly one 'Tier'. Useful when the
-- orchestrator needs the tier identity for reporting.
agentToTier :: Agent -> Tier
agentToTier = \case
    AgentMain      -> TierSentry
    AgentWorkhorse -> TierWorkhorse
    AgentThinker   -> TierThinker
    AgentBuilder   -> TierBuilder

-- | Not every 'Tier' corresponds to an 'Agent' (services / docker /
-- ollama / etc. are non-agent tiers). Deliberately written exhaustively
-- rather than with a catch-all @_ -> Nothing@ so the compiler flags us
-- when a new tier is added and we forget to classify it.
tierToAgent :: Tier -> Maybe Agent
tierToAgent = \case
    TierSentry    -> Just AgentMain
    TierWorkhorse -> Just AgentWorkhorse
    TierThinker   -> Just AgentThinker
    TierBuilder   -> Just AgentBuilder
    TierServices  -> Nothing
    TierDocker    -> Nothing
    TierOllama    -> Nothing
    TierAudit     -> Nothing
    TierConfig    -> Nothing
    TierErrLog    -> Nothing
    TierTelegram  -> Nothing

-- | Outcome of a tier run. Reason is carried with the status rather than
-- as a separate field so we can't represent "Pass with a failure reason".
data TierStatus
    = Pass
    | Fail !Text
    | Skip !Text
    deriving stock (Eq, Show)

isFailure :: TierStatus -> Bool
isFailure = \case
    Pass     -> False
    Fail _   -> True
    Skip _   -> False

-- | One tier's full result: which tier, how it fared, how long it took,
-- and optional extra lines (e.g. a metrics summary or command output).
data TierResult = TierResult
    { trTier    :: !Tier
    , trStatus  :: !TierStatus
    , trElapsed :: !Milliseconds
    , trDetails :: ![Text]
    }
    deriving stock (Eq, Show)

-- | Smart constructors.
pass :: Tier -> Milliseconds -> [Text] -> TierResult
pass t ms ds = TierResult t Pass ms ds

failWith :: Tier -> Milliseconds -> Text -> [Text] -> TierResult
failWith t ms reason ds = TierResult t (Fail reason) ms ds

skipWith :: Tier -> Milliseconds -> Text -> [Text] -> TierResult
skipWith t ms reason ds = TierResult t (Skip reason) ms ds

-- | Summary counts over a run.
data Counts = Counts
    { cPass :: !Int
    , cFail :: !Int
    , cSkip :: !Int
    }
    deriving stock (Eq, Show)

countResults :: [TierResult] -> Counts
countResults = foldr step (Counts 0 0 0)
  where
    step r c = case trStatus r of
        Pass   -> c { cPass = cPass c + 1 }
        Fail _ -> c { cFail = cFail c + 1 }
        Skip _ -> c { cSkip = cSkip c + 1 }

allPassed :: [TierResult] -> Bool
allPassed = all (not . isFailure . trStatus)
