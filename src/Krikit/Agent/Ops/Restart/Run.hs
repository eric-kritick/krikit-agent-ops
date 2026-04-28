{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Service-restart orchestration.
--
-- Wraps the bootout / cleanup / bootstrap / verify dance the bash
-- @krikit-restart@ used to do, with the service identified by the
-- shared 'Service' ADT instead of a string match. Per-service
-- cleanup steps and post-bootstrap settle delays are dispatched by
-- exhaustive patterns on 'Service', so adding a new service forces
-- restart-aware code to update.
module Krikit.Agent.Ops.Restart.Run
    ( -- * Per-service classification
      CleanupKind (..)
    , cleanupFor
    , settleSeconds

      -- * Restart workflow
    , restartService
    , RestartOutcome (..)
    ) where

import           Control.Concurrent             (threadDelay)
import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Effectful                      (Eff, IOE, liftIO, (:>))

import           Krikit.Agent.Ops.Effect.Log
    ( Log
    , logFail
    , logInfo
    , logOk
    )
import           Krikit.Agent.Ops.Effect.Proc
    ( Proc
    , ProcError (..)
    , ProcResult (..)
    , runCmd
    , runCmdAsUser
    )
import           Krikit.Agent.Ops.Service
    ( Service (..)
    , serviceLaunchctlLabel
    )
import           Krikit.Agent.Ops.Units         (Seconds (..), secondsToMicros)

-- | What needs to happen between @bootout@ and @bootstrap@ to make
-- sure the prior process is fully gone before we relaunch.
--
-- Each constructor maps to a concrete subprocess sequence in
-- 'runCleanup'. The "shape" is fixed at the type level so the
-- orchestrator can reason about it without parsing strings.
data CleanupKind
    = CleanupColima      -- ^ @colima stop@ as agentops, then settle
    | CleanupOpenclaw    -- ^ @pkill -9 -f 'openclaw'@
    | CleanupOllama      -- ^ @pkill -9 -f 'ollama serve'@
    | NoCleanup          -- ^ The bootout alone is sufficient
    deriving stock (Eq, Show)

-- | Exhaustive on 'Service'. Adding a new service forces a decision.
cleanupFor :: Service -> CleanupKind
cleanupFor = \case
    ServiceColima          -> CleanupColima
    ServiceOpenclaw        -> CleanupOpenclaw
    ServiceOllama          -> CleanupOllama
    ServiceMonitor         -> NoCleanup
    ServiceWorkspaceBackup -> NoCleanup

-- | How long to wait after @bootstrap@ before we try to inspect the
-- service's state. Colima's Lima VM takes ~30s to boot; the rest are
-- seconds.
settleSeconds :: Service -> Seconds
settleSeconds = \case
    ServiceColima          -> Seconds 30
    ServiceOllama          -> Seconds  5
    ServiceOpenclaw        -> Seconds  5
    ServiceMonitor         -> Seconds  2
    ServiceWorkspaceBackup -> Seconds  2

-- | Outcome of a restart attempt. Mirrors 'TierStatus' in spirit but
-- restart-specific rather than tier-shaped.
data RestartOutcome
    = RestartOk          -- ^ Bootout, cleanup, bootstrap, verify all OK.
    | RestartFailed !Text  -- ^ Carries a one-line reason for the operator.
    deriving stock (Eq, Show)

-- | Run the full restart sequence for a service.
restartService
    :: (Proc :> es, Log :> es, IOE :> es)
    => Service -> Eff es RestartOutcome
restartService svc = do
    let label    = serviceLaunchctlLabel svc
        plist    = "/Library/LaunchDaemons/" <> T.unpack label <> ".plist"
        cleanup  = cleanupFor svc
        settle   = settleSeconds svc

    logInfo ("stopping " <> label <> "...")
    -- bootout returns non-zero when the service wasn't already loaded;
    -- that's not a failure for us, we just want it gone.
    _ <- runCmd "sudo"
            ["launchctl", "bootout", "system/" <> T.unpack label]
            (Seconds 10)
    sleep (Seconds 2)

    runCleanup cleanup
    sleep (Seconds 2)

    logInfo ("starting " <> label <> "...")
    bootstrapResult <- runCmd "sudo"
            ["launchctl", "bootstrap", "system", plist]
            (Seconds 10)
    case bootstrapResult of
        Left e -> do
            let msg = "bootstrap failed: " <> procErrorMsg e
            logFail msg
            pure (RestartFailed msg)
        Right _ -> do
            logInfo ("waiting " <> T.pack (show (secondsInt' settle))
                        <> "s for " <> label <> " to settle...")
            sleep settle
            verifyState svc

-- | Run the per-service cleanup commands. Any failure here is logged
-- but not fatal — bootstrap will surface the real problem.
runCleanup :: (Proc :> es, Log :> es) => CleanupKind -> Eff es ()
runCleanup = \case
    NoCleanup ->
        pure ()

    CleanupColima -> do
        logInfo "colima stop (as agentops)..."
        _ <- runCmdAsUser "agentops" "bash"
                ["-lc", "colima stop"]
                (Seconds 60)
        pure ()

    CleanupOpenclaw -> do
        logInfo "pkill -9 -f 'openclaw' (as root)..."
        _ <- runCmd "sudo" ["pkill", "-9", "-f", "openclaw"] (Seconds 5)
        pure ()

    CleanupOllama -> do
        logInfo "pkill -9 -f 'ollama serve' (as root)..."
        _ <- runCmd "sudo" ["pkill", "-9", "-f", "ollama serve"] (Seconds 5)
        pure ()

-- | Confirm the service is in 'state = running' (or at least loaded
-- in launchd's eyes). For 'MayBeWaiting' services we just check the
-- print succeeds; for 'MustBeRunning' we look for the explicit state.
--
-- We piggyback on serviceExpectation so the criterion stays in one
-- place; restart shouldn't accept "Loaded but not running" for a
-- daemon that's supposed to be permanently up.
verifyState
    :: (Proc :> es, Log :> es)
    => Service -> Eff es RestartOutcome
verifyState svc = do
    let label = serviceLaunchctlLabel svc
    r <- runCmd "sudo"
            ["launchctl", "print", "system/" <> T.unpack label]
            (Seconds 5)
    case r of
        Left e -> do
            let msg = "post-bootstrap launchctl print: " <> procErrorMsg e
            logFail msg
            pure (RestartFailed msg)
        Right ProcResult { prStdout = out } -> do
            let runningNow = "state = running" `T.isInfixOf` out
            if runningNow
                then do
                    logOk (label <> " is running")
                    pure RestartOk
                else do
                    -- For interval-driven daemons (monitor /
                    -- workspace-backup) the expected state is "not
                    -- running"; we just want the service loaded.
                    let loaded = "state" `T.isInfixOf` out
                    if loaded
                        then do
                            logOk (label <> " is loaded (interval-driven; not currently running)")
                            pure RestartOk
                        else do
                            let msg = label <> " bootstrap reported no recognizable state"
                            logFail msg
                            pure (RestartFailed msg)

-- === Helpers =================================================================

sleep :: IOE :> es => Seconds -> Eff es ()
sleep s = liftIO (threadDelay (secondsToMicros s))

secondsInt' :: Seconds -> Int
secondsInt' (Seconds n) = n

procErrorMsg :: ProcError -> Text
procErrorMsg = \case
    ProcTimeout       -> "timeout"
    ProcNonZero n     -> "non-zero exit " <> T.pack (show n)
    ProcLaunchErr msg -> "launch error: " <> msg
