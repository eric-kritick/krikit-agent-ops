{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Effectful orchestrator for @krikit-monitor@.
--
-- One invocation does:
--
--   1. Load state from disk.
--   2. Run all checks.
--   3. Compute alert transitions vs prior state, send each via
--      Telegram.
--   4. If now is the configured digest hour and we haven't fired
--      a digest today, build + send the daily digest. The digest
--      includes the live check results plus the outputs of
--      shelled-out adjunct binaries (@krikit-regen-summary@,
--      @krikit-update-status@).
--   5. Save updated state.
--
-- Mirrors @monitor.py@'s control flow 1:1, with each section now
-- typed and testable.
module Krikit.Agent.Ops.Monitor.Run
    ( -- * Configuration
      RunConfig (..)
    , defaultRunConfig

      -- * Orchestrator
    , runOnce
    ) where

import           Control.Exception                (IOException, try)
import           Control.Monad                    (void)
import qualified Data.Text                        as T
import           Data.Text                        (Text)
import qualified Data.Text.Encoding               as TE
import qualified Data.Text.Encoding.Error         as TE
import           Data.Time                        (LocalTime (..),
                                                   TimeOfDay (..),
                                                   getCurrentTime,
                                                   getCurrentTimeZone,
                                                   showGregorian, utcToLocalTime)
import qualified Data.Time                        as Time
import qualified Data.ByteString.Lazy             as LBS
import           System.Exit                      (ExitCode (..))
import qualified System.Process.Typed             as TP

import           Effectful                        (Eff, IOE, liftIO, (:>))

import           Krikit.Agent.Ops.Effect.Telegram (Telegram, sendMessage)
import           Krikit.Agent.Ops.Monitor.Check   (runAllChecks)
import           Krikit.Agent.Ops.Monitor.Digest
    ( DigestSchedule (..)
    , buildDigest
    , defaultDigestSchedule
    , renderAlert
    , shouldEmitDigest
    , transitionAlerts
    )
import           Krikit.Agent.Ops.Monitor.State
    ( MonitorState (..)
    , defaultStatePath
    , loadStateFrom
    , markDigestEmitted
    , recordResults
    , saveStateTo
    )

-- =============================================================================
-- Configuration
-- =============================================================================

-- | Per-invocation knobs. Default values match
-- @macmini-monitoring-playbook.md@.
data RunConfig = RunConfig
    { rcStatePath        :: !FilePath
    , rcDigestSchedule   :: !DigestSchedule
    , rcRegenSummaryBin  :: !(Maybe FilePath)
    , rcUpdateStatusBin  :: !(Maybe FilePath)
    , rcHostName         :: !Text
    }
    deriving stock (Show)

-- | Defaults: match the on-mini layout. Adjuncts default to the
-- canonical install paths under @\/usr\/local\/bin\/krikit\/@.
defaultRunConfig :: RunConfig
defaultRunConfig = RunConfig
    { rcStatePath       = defaultStatePath
    , rcDigestSchedule  = defaultDigestSchedule
    , rcRegenSummaryBin = Just "/usr/local/bin/krikit/krikit-regen-summary"
    , rcUpdateStatusBin = Just "/usr/local/bin/krikit/krikit-update-status"
    , rcHostName        = "krikit-agent-001"
    }

-- =============================================================================
-- Orchestrator
-- =============================================================================

-- | One full monitor pass: run checks, alert on transitions,
-- maybe emit digest, persist state. Returns nothing -- the side
-- effects are the point. Failures inside any one step are
-- swallowed so the pass continues (matches @monitor.py@'s
-- always-exit-0 stance: alerting is best-effort, launchd will
-- fire us again in 5 minutes).
runOnce
    :: ( Telegram :> es
       , IOE     :> es
       )
    => RunConfig
    -> Eff es ()
runOnce cfg = do
    -- 1. Load state.
    state0 <- liftIO (loadStateFrom (rcStatePath cfg))

    -- 2. Run checks.
    results <- runAllChecks
    nowIso  <- liftIO isoUtcNow
    let stateA = recordResults nowIso results state0

    -- 3. Transition alerts (one Telegram message per transition).
    let alerts = transitionAlerts (msAlerts state0) results
    mapM_ (void . sendMessage . renderAlert) alerts

    -- 4. Daily digest, gated by hour + last-emitted-date.
    (nowHourLocal, todayLocalIso) <- liftIO localHourAndDate
    stateB <-
        if shouldEmitDigest (rcDigestSchedule cfg)
                            nowHourLocal
                            todayLocalIso
                            stateA
            then do
                regen  <- liftIO (readBin (rcRegenSummaryBin  cfg))
                upd    <- liftIO (readBin (rcUpdateStatusBin  cfg))
                let body = buildDigest todayLocalIso (rcHostName cfg) results
                            [ ("Regen / verify", regen)
                            , ("Updates",        upd)
                            ]
                void (sendMessage body)
                pure (markDigestEmitted todayLocalIso stateA)
            else pure stateA

    -- 5. Persist.
    liftIO (saveStateTo (rcStatePath cfg) stateB)

-- =============================================================================
-- Helpers
-- =============================================================================

isoUtcNow :: IO Text
isoUtcNow = do
    t <- getCurrentTime
    pure (T.pack (Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t))

-- | (current local hour 0..23, today's date as @YYYY-MM-DD@).
localHourAndDate :: IO (Int, Text)
localHourAndDate = do
    tz  <- getCurrentTimeZone
    now <- getCurrentTime
    let LocalTime { localDay = day, localTimeOfDay = tod } =
            utcToLocalTime tz now
    pure (todHour tod, T.pack (showGregorian day))

-- | Best-effort read of an adjunct binary's stdout. On any IO
-- failure (binary missing, non-zero exit, decode error) returns
-- empty 'Text'; the digest assembly drops empty sections.
readBin :: Maybe FilePath -> IO Text
readBin = \case
    Nothing  -> pure ""
    Just bin -> do
        r <- try @IOException $
            TP.readProcessStdout (TP.proc bin [])
        case r of
            Right (ExitSuccess, body) ->
                pure (decodeLenient body)
            _ -> pure ""

decodeLenient :: LBS.ByteString -> Text
decodeLenient = TE.decodeUtf8With TE.lenientDecode . LBS.toStrict
