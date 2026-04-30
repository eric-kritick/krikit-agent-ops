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
--   4. Decide whether to fire the daily digest. The decision is
--      pure and lives in 'decideDigest':
--
--       * Normal launchd-driven tick (@'NormalSchedule'@): fire
--         only if the hour gate matches and we haven't fired
--         today; mark today as emitted post-fire.
--       * Operator-invoked @--force-digest@
--         (@'ForceDigest'@): fire unconditionally; DO NOT mark
--         today as emitted, so the regularly scheduled digest
--         still fires at @dsHourLocal@.
--
--      The digest includes the live check results plus the
--      outputs of shelled-out adjunct binaries
--      (@krikit-regen-summary@, @krikit-update-status@).
--   5. Save updated state.
--
-- Mirrors @monitor.py@'s control flow 1:1, with each section now
-- typed and testable.
module Krikit.Agent.Ops.Monitor.Run
    ( -- * Configuration
      RunConfig (..)
    , defaultRunConfig
    , runConfigFromEnv

      -- * Orchestrator
    , runOnce
    , runOnceWith
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

import           System.Environment                (lookupEnv)
import           Text.Read                         (readMaybe)

import           Krikit.Agent.Ops.Effect.Telegram (Telegram, sendMessage)
import           Krikit.Agent.Ops.Monitor.Check   (runAllChecks)
import           Krikit.Agent.Ops.Monitor.Digest
    ( DigestDecision (..)
    , DigestSchedule (..)
    , FireMode (..)
    , ForceDigest (..)
    , buildDigest
    , decideDigest
    , defaultDigestSchedule
    , renderAlert
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

-- | Build a 'RunConfig' from 'defaultRunConfig' overlaid with
-- env-var overrides. Recognized variables:
--
--   * @KRIKIT_MONITOR_HOST_NAME@   -- digest header host name
--   * @KRIKIT_MONITOR_DIGEST_HOUR@ -- 0..23, local time
--
-- Empty / missing / unparseable values fall through to the
-- default. Sourced from @\/Users\/agentops\/.config\/channels.env@
-- by the wrapper script.
runConfigFromEnv :: IO RunConfig
runConfigFromEnv = do
    mHost <- lookupEnv "KRIKIT_MONITOR_HOST_NAME"
    mHour <- lookupEnv "KRIKIT_MONITOR_DIGEST_HOUR"
    let base = defaultRunConfig
        host = case nonEmpty mHost of
            Just s  -> T.pack s
            Nothing -> rcHostName base
        sched = case nonEmpty mHour >>= readMaybe of
            Just h | 0 <= h && h <= 23 ->
                (rcDigestSchedule base) { dsHourLocal = h }
            _ -> rcDigestSchedule base
    pure base
        { rcHostName       = host
        , rcDigestSchedule = sched
        }
  where
    nonEmpty (Just s) | not (null s) = Just s
    nonEmpty _                       = Nothing

-- =============================================================================
-- Orchestrator
-- =============================================================================

-- | One full monitor pass under the normal launchd-driven
-- schedule. See 'runOnceWith' for the underlying primitive.
runOnce
    :: ( Telegram :> es
       , IOE     :> es
       )
    => RunConfig
    -> Eff es ()
runOnce = runOnceWith NormalSchedule

-- | One full monitor pass: run checks, alert on transitions,
-- maybe emit digest, persist state. Returns nothing -- the side
-- effects are the point. Failures inside any one step are
-- swallowed so the pass continues (matches @monitor.py@'s
-- always-exit-0 stance: alerting is best-effort, launchd will
-- fire us again in 5 minutes).
--
-- 'ForceDigest' inverts the digest gate (see module docstring
-- and 'decideDigest'); a forced fire does NOT mark the day as
-- emitted, so a normal schedule digest tomorrow still fires.
runOnceWith
    :: ( Telegram :> es
       , IOE     :> es
       )
    => ForceDigest
    -> RunConfig
    -> Eff es ()
runOnceWith force cfg = do
    -- 1. Load state.
    state0 <- liftIO (loadStateFrom (rcStatePath cfg))

    -- 2. Run checks.
    results <- runAllChecks
    nowIso  <- liftIO isoUtcNow
    let stateA = recordResults nowIso results state0

    -- 3. Transition alerts (one Telegram message per transition).
    let alerts = transitionAlerts (msAlerts state0) results
    mapM_ (void . sendMessage . renderAlert) alerts

    -- 4. Digest decision (pure). Then act on it.
    (nowHourLocal, todayLocalIso) <- liftIO localHourAndDate
    let decision = decideDigest force
                                (rcDigestSchedule cfg)
                                nowHourLocal
                                todayLocalIso
                                stateA
    stateB <- case decision of
        DigestSkip -> pure stateA
        DigestFire mode -> do
            regen  <- liftIO (readBin (rcRegenSummaryBin  cfg))
            upd    <- liftIO (readBin (rcUpdateStatusBin  cfg))
            let body = buildDigest todayLocalIso (rcHostName cfg) results
                        [ ("Regen / verify", regen)
                        , ("Updates",        upd)
                        ]
            void (sendMessage body)
            pure $ case mode of
                ScheduledFire -> markDigestEmitted todayLocalIso stateA
                ForcedFire    -> stateA

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
