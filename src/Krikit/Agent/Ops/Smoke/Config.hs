{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
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
    , timeoutFor

    , Paths (..)
    , defaultPaths

    , Thresholds (..)
    , defaultThresholds
    ) where

import           Krikit.Agent.Ops.Smoke.Tier (Agent (..))
import           Krikit.Agent.Ops.Units      (Seconds (..))

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
        { scTimeouts   = defaultTimeouts
        , scPaths      = defaultPaths
        , scThresholds = defaultThresholds
        }

-- | Per-tier timeout ceilings. Values match the bash script.
data Timeouts = Timeouts
    { toSentry       :: !Seconds
    , toWorkhorse    :: !Seconds
    , toThinker      :: !Seconds
    , toBuilder      :: !Seconds
    , toTelegramWait :: !Seconds  -- how long to wait for nonce in the log
    , toShortCmd     :: !Seconds  -- short commands (launchctl, docker info, ...)
    }
    deriving stock (Eq, Show)

defaultTimeouts :: Timeouts
defaultTimeouts =
    Timeouts
        { toSentry       = Seconds 30
        , toWorkhorse    = Seconds 45
        , toThinker      = Seconds 120
        , toBuilder      = Seconds 120
        , toTelegramWait = Seconds 60
        , toShortCmd     = Seconds 10
        }

-- | Type-safe lookup: which timeout field applies to which agent.
-- Exhaustive on 'Agent', so adding a new agent forces update here.
timeoutFor :: Timeouts -> Agent -> Seconds
timeoutFor t = \case
    AgentMain      -> toSentry    t
    AgentWorkhorse -> toWorkhorse t
    AgentThinker   -> toThinker   t
    AgentBuilder   -> toBuilder   t

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
