{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | The set of LaunchDaemons that make up the agent host.
--
-- Lives at the top of the namespace because multiple binaries care:
-- the smoke test classifies them in 'TierServices', the restart tool
-- dispatches on them, the health tool reports on them. Each of those
-- pulls in this module rather than redefining the set.
module Krikit.Agent.Ops.Service
    ( -- * Identity
      Service (..)
    , allServices
    , serviceLaunchctlLabel
    , serviceShortName
    , serviceFromShortName

      -- * Expected runtime state
    , ServiceExpectation (..)
    , serviceExpectation
    ) where

import Data.Text (Text)

-- | A LaunchDaemon under @ai.krikit.*@. Carrying these as ADT
-- constructors instead of raw 'Text' prevents typo bugs
-- (@ai.krikit.colima@ vs @ai.krikit.Colima@) and forces compile-time
-- update on every dispatch site when we add or rename a service.
data Service
    = ServiceColima
    | ServiceOllama
    | ServiceOpenclaw
    | ServiceMonitor
    | ServiceWorkspaceBackup
    deriving stock (Eq, Show, Bounded, Enum)

-- | Iteration-friendly list of every service. Equivalent to
-- @[minBound .. maxBound]@ but reads better.
allServices :: [Service]
allServices = [minBound .. maxBound]

-- | The exact label launchctl expects, e.g. as in
-- @launchctl print system/ai.krikit.colima@.
serviceLaunchctlLabel :: Service -> Text
serviceLaunchctlLabel = \case
    ServiceColima          -> "ai.krikit.colima"
    ServiceOllama          -> "ai.krikit.ollama"
    ServiceOpenclaw        -> "ai.krikit.openclaw"
    ServiceMonitor         -> "ai.krikit.monitor"
    ServiceWorkspaceBackup -> "ai.krikit.workspace-backup"

-- | The short name the operator types on the command line
-- (@krikit-restart colima@). Stable; promised to never change.
serviceShortName :: Service -> Text
serviceShortName = \case
    ServiceColima          -> "colima"
    ServiceOllama          -> "ollama"
    ServiceOpenclaw        -> "openclaw"
    ServiceMonitor         -> "monitor"
    ServiceWorkspaceBackup -> "workspace-backup"

-- | Parse an operator-typed short name back into a 'Service'.
-- Returns 'Nothing' for unrecognized input; the caller decides how
-- to render the error (CLI usage block, etc.).
serviceFromShortName :: Text -> Maybe Service
serviceFromShortName = \case
    "colima"           -> Just ServiceColima
    "ollama"           -> Just ServiceOllama
    "openclaw"         -> Just ServiceOpenclaw
    "monitor"          -> Just ServiceMonitor
    "workspace-backup" -> Just ServiceWorkspaceBackup
    _                  -> Nothing

-- === Expected state ==========================================================

-- | What we expect a given service's runtime state to look like.
-- Long-running daemons are 'MustBeRunning'; one-shot boot helpers and
-- scheduled jobs are normally @not running@ between fires
-- ('MayBeWaiting'), and treating those as failures would be a
-- false positive in any health-style check.
data ServiceExpectation
    = MustBeRunning
    | MayBeWaiting
    deriving stock (Eq, Show)

-- | Exhaustive on 'Service'. Adding a new service forces a
-- classification decision here.
serviceExpectation :: Service -> ServiceExpectation
serviceExpectation = \case
    ServiceColima          -> MayBeWaiting    -- RunAtLoad once; VM survives
    ServiceOllama          -> MustBeRunning   -- always on
    ServiceOpenclaw        -> MustBeRunning   -- always on
    ServiceMonitor         -> MayBeWaiting    -- every 5 min
    ServiceWorkspaceBackup -> MayBeWaiting    -- daily 05:30
