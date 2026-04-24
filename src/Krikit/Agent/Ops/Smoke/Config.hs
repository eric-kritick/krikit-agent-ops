{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Hardcoded configuration values for the smoke test.
--
-- Timeouts, filesystem paths, expected thresholds. No IO; pure values.
-- If/when these need to become operator-tunable, extract to a config file
-- and keep this module as the defaults.
module Krikit.Agent.Ops.Smoke.Config
    ( SmokeConfig (..)
    , defaultConfig
    , Timeouts (..)
    , defaultTimeouts
    , Paths (..)
    , defaultPaths
    , Thresholds (..)
    , defaultThresholds
    , AgentLabel (..)
    , allServices
    , agentLabels
    ) where

import Data.Text (Text)

-- | Top-level config bundle. Consumed by 'Krikit.Agent.Ops.Smoke.Run'.
data SmokeConfig = SmokeConfig
    { scTimeouts   :: !Timeouts
    , scPaths      :: !Paths
    , scThresholds :: !Thresholds
    }
    deriving stock (Eq, Show)

defaultConfig :: SmokeConfig
defaultConfig =
    SmokeConfig
        { scTimeouts = defaultTimeouts
        , scPaths = defaultPaths
        , scThresholds = defaultThresholds
        }

-- | Per-tier timeout ceilings. Values match the bash script.
data Timeouts = Timeouts
    { toSentry       :: !Int  -- seconds
    , toWorkhorse    :: !Int
    , toThinker      :: !Int
    , toBuilder      :: !Int
    , toTelegramWait :: !Int  -- how long to wait for nonce to appear in log
    , toShortCmd     :: !Int  -- short commands (launchctl, docker info, ...)
    }
    deriving stock (Eq, Show)

defaultTimeouts :: Timeouts
defaultTimeouts =
    Timeouts
        { toSentry       = 30
        , toWorkhorse    = 45
        , toThinker      = 120
        , toBuilder      = 120
        , toTelegramWait = 60
        , toShortCmd     = 10
        }

-- | Filesystem paths the smoke test reads from.
data Paths = Paths
    { pOpenclawConfig :: !FilePath
    , pOpenclawErrLog :: !FilePath
    , pChannelsEnv    :: !FilePath
    , pColimaSocket   :: !FilePath
    , pHistoryLog     :: !FilePath
    , pOpenclawLogDir :: !FilePath
    }
    deriving stock (Eq, Show)

defaultPaths :: Paths
defaultPaths =
    Paths
        { pOpenclawConfig = "/Users/agentops/.openclaw/openclaw.json"
        , pOpenclawErrLog = "/var/log/krikit/openclaw.err.log"
        , pChannelsEnv    = "/Users/agentops/.config/channels.env"
        , pColimaSocket   = "/Users/agentops/.colima/default/docker.sock"
        , pHistoryLog     = "/var/log/krikit/smoke.log"
        , pOpenclawLogDir = "/tmp/openclaw"
        }

-- | Minimum thresholds that must be satisfied to count as a pass.
data Thresholds = Thresholds
    { thMinOllamaModels      :: !Int
    , thMinLaunchdServices   :: !Int
    , thErrLogScanLineCount  :: !Int
    }
    deriving stock (Eq, Show)

defaultThresholds :: Thresholds
defaultThresholds =
    Thresholds
        { thMinOllamaModels     = 4
        , thMinLaunchdServices  = 5
        , thErrLogScanLineCount = 200
        }

-- | Expected launchd services. One entry per 'Tier's "services" check.
allServices :: [Text]
allServices =
    [ "ai.krikit.colima"
    , "ai.krikit.ollama"
    , "ai.krikit.openclaw"
    , "ai.krikit.monitor"
    , "ai.krikit.workspace-backup"
    ]

-- | Labels used by agent tiers when invoking @openclaw agent --agent <name>@.
newtype AgentLabel = AgentLabel Text
    deriving stock   (Eq, Show)
    deriving newtype (Ord)

agentLabels :: [(String, AgentLabel)]
agentLabels =
    [ ("main",      AgentLabel "main")
    , ("workhorse", AgentLabel "workhorse")
    , ("thinker",   AgentLabel "thinker")
    , ("builder",   AgentLabel "builder")
    ]
