{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | The four agents the OpenClaw gateway exposes.
--
-- Lives at the top of the namespace because more than one binary
-- cares — smoke probes them, future tools may dispatch on them. The
-- mapping to a smoke 'Tier' lives in "Krikit.Agent.Ops.Smoke.Tier"
-- (where the Tier sum lives), not here.
module Krikit.Agent.Ops.Agent
    ( Agent (..)
    , allAgents
    , agentOpenclawName
    , agentDisplayName
    ) where

import Data.Text (Text)

-- | One of the four agents the OpenClaw gateway exposes via
-- @openclaw agent --agent \<name\>@.
data Agent
    = AgentMain       -- ^ Sentry persona (Wicket)
    | AgentWorkhorse  -- ^ Montag
    | AgentThinker    -- ^ Faber
    | AgentBuilder    -- ^ Stendahl
    deriving stock (Eq, Show, Bounded, Enum)

-- | Iteration-friendly list of every agent.
allAgents :: [Agent]
allAgents = [minBound .. maxBound]

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
