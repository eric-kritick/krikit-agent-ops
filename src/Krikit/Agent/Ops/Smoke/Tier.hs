{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Tier domain types for the smoke test.
--
-- The 'Tier' sum is smoke-specific. The 'Service' and 'Agent' ADTs
-- are shared across binaries and live in their own modules
-- ("Krikit.Agent.Ops.Service" and "Krikit.Agent.Ops.Agent"); this
-- module just declares the smoke-specific Tier <-> Agent mapping.
module Krikit.Agent.Ops.Smoke.Tier
    ( -- * Tier identity
      Tier (..)
    , tierName
    , tierKey

      -- * Tier <-> Agent mapping (smoke-specific)
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

import Krikit.Agent.Ops.Agent (Agent (..))
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

-- === Tier <-> Agent mapping ==================================================

-- | Every 'Agent' corresponds to exactly one 'Tier'. Used by the
-- orchestrator to attach an agent's result to the right tier.
agentToTier :: Agent -> Tier
agentToTier = \case
    AgentMain      -> TierSentry
    AgentWorkhorse -> TierWorkhorse
    AgentThinker   -> TierThinker
    AgentBuilder   -> TierBuilder

-- | Not every 'Tier' corresponds to an 'Agent' (services / docker /
-- ollama / etc. are non-agent tiers). Deliberately written exhaustively
-- rather than with a catch-all @_ -> Nothing@ so the compiler flags
-- us when a new tier is added and we forget to classify it.
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

-- === Status / result =========================================================

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

-- === Aggregate counts ========================================================

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
