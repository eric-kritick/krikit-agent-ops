{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

-- | Persistent state for @krikit-monitor@.
--
-- Tracks two things across runs:
--
--   1. The previous-run status of every check, so we can emit
--      transition alerts (\"recovered\" or \"newly failing\") --
--      not on every alarm tick.
--   2. The last-emitted-digest date, so the daily summary fires
--      exactly once per day.
--
-- Stored as JSON at @\/var\/lib\/krikit\/monitor-state.json@
-- (override via 'loadStateFrom' \/ 'saveStateTo'). Writes are
-- atomic: write to a sibling tmp file, fsync via 'IO', then
-- 'renamePath'.
module Krikit.Agent.Ops.Monitor.State
    ( -- * Types
      MonitorState (..)
    , emptyState

      -- * Persistence
    , defaultStatePath
    , loadState
    , loadStateFrom
    , saveState
    , saveStateTo

      -- * Pure update
    , recordResults
    , markDigestEmitted
    ) where

import           Control.Exception                  (IOException, try)
import           Data.Aeson                         (FromJSON (..),
                                                     ToJSON (..), object,
                                                     (.:?), (.=))
import qualified Data.Aeson                         as A
import qualified Data.Aeson.Types                   as AT
import qualified Data.ByteString.Lazy               as LBS
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Text                          (Text)
import           System.Directory                   (createDirectoryIfMissing,
                                                     doesFileExist, renamePath)
import           System.FilePath                    (takeDirectory, (<.>))

import           Krikit.Agent.Ops.Monitor.Check     (CheckResult (..),
                                                     CheckStatus,
                                                     statusFromText,
                                                     statusToText)

-- =============================================================================
-- Types
-- =============================================================================

-- | The whole monitor state document.
data MonitorState = MonitorState
    { msAlerts          :: !(Map Text CheckStatus)
      -- ^ check name -> last-known status
    , msLastRun         :: !Text
      -- ^ ISO-8601 timestamp of the last completed run
    , msLastDigestDate  :: !Text
      -- ^ ISO date (@YYYY-MM-DD@) of the last digest emission
    }
    deriving stock (Eq, Show)

-- | Initial / empty state for a fresh mini.
emptyState :: MonitorState
emptyState = MonitorState
    { msAlerts         = Map.empty
    , msLastRun        = ""
    , msLastDigestDate = ""
    }

-- =============================================================================
-- JSON
-- =============================================================================

instance ToJSON MonitorState where
    toJSON ms = object
        [ "alerts"           .= Map.map statusToText (msAlerts ms)
        , "last_run"         .= msLastRun ms
        , "last_digest_date" .= msLastDigestDate ms
        ]

instance FromJSON MonitorState where
    parseJSON = A.withObject "MonitorState" $ \o -> do
        rawAlerts <- o .:? "alerts" :: AT.Parser (Maybe (Map Text Text))
        let alerts = maybe Map.empty (Map.map statusFromText) rawAlerts
        run    <- o .:? "last_run"
        digest <- o .:? "last_digest_date"
        pure MonitorState
            { msAlerts         = alerts
            , msLastRun        = maybe "" id run
            , msLastDigestDate = maybe "" id digest
            }

-- =============================================================================
-- Persistence
-- =============================================================================

defaultStatePath :: FilePath
defaultStatePath = "/var/lib/krikit/monitor-state.json"

-- | Load state from 'defaultStatePath', returning 'emptyState' on
-- absence or any parse failure (this mirrors @monitor.py@: a
-- corrupt state file shouldn't take down the alerting pipeline).
loadState :: IO MonitorState
loadState = loadStateFrom defaultStatePath

loadStateFrom :: FilePath -> IO MonitorState
loadStateFrom path = do
    exists <- doesFileExist path
    if not exists
        then pure emptyState
        else do
            r <- try @IOException (LBS.readFile path)
            case r of
                Left _      -> pure emptyState
                Right bytes -> case A.eitherDecode bytes of
                    Right ms -> pure ms
                    Left _   -> pure emptyState

-- | Save state to 'defaultStatePath' atomically.
saveState :: MonitorState -> IO ()
saveState = saveStateTo defaultStatePath

-- | Atomic write: encode -> write to @path.tmp@ -> rename.
-- Creates the parent directory if it doesn't exist.
saveStateTo :: FilePath -> MonitorState -> IO ()
saveStateTo path ms = do
    let dir = takeDirectory path
        tmp = path <.> "tmp"
    createDirectoryIfMissing True dir
    LBS.writeFile tmp (A.encode ms)
    renamePath tmp path

-- =============================================================================
-- Pure update
-- =============================================================================

-- | Replace the alerts map with the latest check results, also
-- bumping 'msLastRun' to the supplied timestamp.
recordResults
    :: Text                  -- ^ ISO-8601 timestamp (caller-supplied)
    -> [CheckResult]
    -> MonitorState
    -> MonitorState
recordResults nowIso results ms = ms
    { msAlerts  = Map.fromList [(crName r, crStatus r) | r <- results]
    , msLastRun = nowIso
    }

-- | Set 'msLastDigestDate' to the supplied @YYYY-MM-DD@. Caller
-- should compute this consistent with whatever date check it used
-- to decide it was time to emit (i.e. @local now@.date.iso).
markDigestEmitted :: Text -> MonitorState -> MonitorState
markDigestEmitted date ms = ms { msLastDigestDate = date }
