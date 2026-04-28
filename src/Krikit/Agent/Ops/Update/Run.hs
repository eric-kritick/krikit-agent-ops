{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Unified updater for the agent-stack CLIs.
--
-- Subsumes the four bash @krikit-update-*@ scripts (claude, codex,
-- openclaw, ollama). Single source of truth for "how do I update
-- tool X." Each per-tool runner records before/after versions and
-- (for openclaw / ollama) restarts the relevant LaunchDaemon so
-- the new binary is actually picked up.
module Krikit.Agent.Ops.Update.Run
    ( -- * Domain types
      UpdateTarget (..)
    , allTargets
    , targetShortName
    , targetFromShortName
    , UpdateOutcome (..)
    , UpdateStatus (..)

      -- * Orchestrators
    , updateTarget
    , updateAll

      -- * Reporting
    , renderOutcome
    , renderOutcomes
    ) where

import           Data.Text                              (Text)
import qualified Data.Text                              as T

import           Effectful                              (Eff, IOE, (:>))

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
import           Krikit.Agent.Ops.Restart.Run
    ( RestartOutcome (..)
    , restartService
    )
import           Krikit.Agent.Ops.Service               (Service (..))
import           Krikit.Agent.Ops.Units                 (Seconds (..))
import           Krikit.Agent.Ops.UpdateStatus.Version
    ( Version (..)
    , extractVersion
    )

-- | What we know how to update. ADT, not stringly-typed.
data UpdateTarget
    = UpdateClaude     -- ^ Claude Code CLI; uses its built-in updater
    | UpdateCodex      -- ^ @\@openai\/codex@ via npm (both users)
    | UpdateOpenclaw   -- ^ @openclaw@ via npm (agentops); restart service
    | UpdateOllama     -- ^ @ollama@ via Homebrew; restart service
    deriving stock (Eq, Show, Bounded, Enum)

-- | Iteration order matches the bash @krikit-update-all@'s safe
-- order: cheap CLIs first, openclaw last so its restart picks up
-- already-updated subprocess CLIs.
allTargets :: [UpdateTarget]
allTargets =
    [ UpdateOllama
    , UpdateClaude
    , UpdateCodex
    , UpdateOpenclaw
    ]

targetShortName :: UpdateTarget -> Text
targetShortName = \case
    UpdateClaude   -> "claude"
    UpdateCodex    -> "codex"
    UpdateOpenclaw -> "openclaw"
    UpdateOllama   -> "ollama"

targetFromShortName :: Text -> Maybe UpdateTarget
targetFromShortName = \case
    "claude"   -> Just UpdateClaude
    "codex"    -> Just UpdateCodex
    "openclaw" -> Just UpdateOpenclaw
    "ollama"   -> Just UpdateOllama
    _          -> Nothing

-- | Outcome of a single tool's update attempt.
data UpdateOutcome = UpdateOutcome
    { uoTarget :: !UpdateTarget
    , uoBefore :: !(Maybe Version)
    , uoAfter  :: !(Maybe Version)
    , uoStatus :: !UpdateStatus
    }
    deriving stock (Eq, Show)

data UpdateStatus
    = UpdateChanged
    -- ^ Before /= after; the update did something.
    | UpdateUnchanged
    -- ^ Before == after; tool was already at latest.
    | UpdateRestartFailed !Text
    -- ^ npm install / brew upgrade succeeded but the service
    -- restart didn't.
    | UpdateFailed !Text
    -- ^ Subprocess error; details in payload.
    deriving stock (Eq, Show)

-- | Standard timeout for npm / brew operations. Most are fast but a
-- cold cache or slow registry can push past 30s.
updateTimeout :: Seconds
updateTimeout = Seconds 180

versionTimeout :: Seconds
versionTimeout = Seconds 5

-- === Orchestrators ===========================================================

-- | Update a single tool. Wraps the per-target runner and
-- ensures we always return an 'UpdateOutcome'.
updateTarget
    :: (Proc :> es, Log :> es, IOE :> es)
    => UpdateTarget -> Eff es UpdateOutcome
updateTarget t = do
    logInfo ("== updating " <> targetShortName t <> " ==")
    case t of
        UpdateClaude   -> updateClaude
        UpdateCodex    -> updateCodex
        UpdateOpenclaw -> updateOpenclaw
        UpdateOllama   -> updateOllama

-- | Update every target in 'allTargets' order. Returns one outcome
-- per target so the caller can render a summary.
updateAll
    :: (Proc :> es, Log :> es, IOE :> es)
    => Eff es [UpdateOutcome]
updateAll = mapM updateTarget allTargets

-- === Per-tool implementations ================================================

-- | Claude Code: built-in @claude update@ for both opsadmin and
-- agentops. Each user has its own auth + its own copy of the
-- updater state, so we run twice.
updateClaude
    :: (Proc :> es, Log :> es)
    => Eff es UpdateOutcome
updateClaude = do
    before <- versionAs "claude" Nothing
    -- opsadmin
    rOps <- runCmd "claude" ["update"] updateTimeout
    failure1 <- logRunResult "claude update (opsadmin)" rOps
    -- agentops
    rAgt <- runCmdAsUser "agentops" "claude" ["update"] updateTimeout
    failure2 <- logRunResult "claude update (agentops)" rAgt
    after  <- versionAs "claude" Nothing
    pure (mkOutcome UpdateClaude before after (firstFailure [failure1, failure2]))

-- | Codex: @npm install -g @openai\/codex@latest@ for both users.
updateCodex
    :: (Proc :> es, Log :> es)
    => Eff es UpdateOutcome
updateCodex = do
    let pkg = "@openai/codex@latest"
    before <- versionAs "codex" Nothing
    rOps <- runCmd "npm" ["install", "-g", pkg] updateTimeout
    f1   <- logRunResult "npm install -g @openai/codex (opsadmin)" rOps
    rAgt <- runCmdAsUser "agentops" "npm" ["install", "-g", pkg] updateTimeout
    f2   <- logRunResult "npm install -g @openai/codex (agentops)" rAgt
    after <- versionAs "codex" Nothing
    pure (mkOutcome UpdateCodex before after (firstFailure [f1, f2]))

-- | OpenClaw: agentops only; npm install + service restart.
updateOpenclaw
    :: (Proc :> es, Log :> es, IOE :> es)
    => Eff es UpdateOutcome
updateOpenclaw = do
    before <- versionAs "openclaw" (Just "agentops")
    rNpm <- runCmdAsUser "agentops" "npm"
                ["install", "-g", "openclaw@latest"]
                updateTimeout
    npmFailure <- logRunResult "npm install -g openclaw (agentops)" rNpm
    after  <- versionAs "openclaw" (Just "agentops")
    case npmFailure of
        Just reason ->
            pure (mkOutcome UpdateOpenclaw before after (Just reason))
        Nothing ->
            -- Restart the daemon so the new binary actually serves.
            restartIfChanged UpdateOpenclaw before after ServiceOpenclaw

-- | Ollama: @brew upgrade ollama@; restart the daemon to load the
-- new binary if anything changed.
updateOllama
    :: (Proc :> es, Log :> es, IOE :> es)
    => Eff es UpdateOutcome
updateOllama = do
    before <- versionAs "ollama" Nothing
    -- brew update first so the formula info is fresh
    _ <- runCmd "brew" ["update"] updateTimeout
    rUp <- runCmd "brew" ["upgrade", "ollama"] updateTimeout
    -- brew exits non-zero when there's no upgrade to do; that's
    -- fine for us.
    upFailure <- case rUp of
        Right _ -> pure Nothing
        Left (ProcNonZero _) -> pure Nothing
        Left e -> do
            logFail (procErrorMsg e)
            pure (Just (procErrorMsg e))
    after <- versionAs "ollama" Nothing
    case upFailure of
        Just reason ->
            pure (mkOutcome UpdateOllama before after (Just reason))
        Nothing ->
            restartIfChanged UpdateOllama before after ServiceOllama

-- === Helpers =================================================================

-- | Run @<cmd> --version@ and pull a semver-like substring.
versionAs
    :: (Proc :> es)
    => FilePath -> Maybe Text -> Eff es (Maybe Version)
versionAs cmd asUser = do
    r <- case asUser of
        Nothing   -> runCmd       cmd ["--version"] versionTimeout
        Just user -> runCmdAsUser user cmd ["--version"] versionTimeout
    pure $ case r of
        Right ProcResult { prStdout = out } -> extractVersion out
        Left _                              -> Nothing

-- | If the version actually changed, restart the named service so
-- the running daemon picks up the new binary. If unchanged, no
-- action.
restartIfChanged
    :: (Proc :> es, Log :> es, IOE :> es)
    => UpdateTarget
    -> Maybe Version
    -> Maybe Version
    -> Service
    -> Eff es UpdateOutcome
restartIfChanged target before after svc =
    if before == after
        then pure (mkOutcome target before after Nothing)
        else do
            logInfo ("version changed; restarting service for " <> targetShortName target)
            outcome <- restartService svc
            case outcome of
                RestartOk ->
                    pure (mkOutcome target before after Nothing)
                RestartFailed reason -> do
                    logFail ("restart failed: " <> reason)
                    pure UpdateOutcome
                        { uoTarget = target
                        , uoBefore = before
                        , uoAfter  = after
                        , uoStatus = UpdateRestartFailed reason
                        }

-- | Materialize an 'UpdateOutcome' from before / after versions and
-- an optional pre-restart failure reason.
mkOutcome
    :: UpdateTarget
    -> Maybe Version
    -> Maybe Version
    -> Maybe Text
    -> UpdateOutcome
mkOutcome t before after = \case
    Just reason ->
        UpdateOutcome t before after (UpdateFailed reason)
    Nothing ->
        UpdateOutcome
            { uoTarget = t
            , uoBefore = before
            , uoAfter  = after
            , uoStatus = if before == after then UpdateUnchanged else UpdateChanged
            }

-- | Convert a subprocess result to a Maybe error reason, logging an
-- OK or FAIL line as a side effect.
logRunResult
    :: (Log :> es)
    => Text
    -> Either ProcError ProcResult
    -> Eff es (Maybe Text)
logRunResult label = \case
    Right _ -> do
        logOk label
        pure Nothing
    Left e -> do
        let msg = procErrorMsg e
        logFail (label <> ": " <> msg)
        pure (Just msg)

firstFailure :: [Maybe Text] -> Maybe Text
firstFailure = \case
    []                 -> Nothing
    Nothing : rest     -> firstFailure rest
    Just reason : _    -> Just reason

procErrorMsg :: ProcError -> Text
procErrorMsg = \case
    ProcTimeout       -> "timeout"
    ProcNonZero n     -> "non-zero exit " <> T.pack (show n)
    ProcLaunchErr msg -> "launch error: " <> msg

-- === Rendering ===============================================================

-- | One-line outcome for the summary block.
renderOutcome :: UpdateOutcome -> Text
renderOutcome o =
    let name   = T.justifyLeft 12 ' ' (targetShortName (uoTarget o))
        before = renderV (uoBefore o)
        after  = renderV (uoAfter o)
    in case uoStatus o of
        UpdateUnchanged ->
            "    " <> name <> after <> " (already latest)"
        UpdateChanged ->
            "  ! " <> name <> before <> " -> " <> after
        UpdateRestartFailed reason ->
            "  ! " <> name <> before <> " -> " <> after
                <> "  ! restart failed: " <> reason
        UpdateFailed reason ->
            "  ! " <> name <> "FAILED: " <> reason
  where
    renderV Nothing  = "?"
    renderV (Just v) = case v of
        Version t -> t

-- | Multi-tool summary block.
renderOutcomes :: [UpdateOutcome] -> Text
renderOutcomes os =
    T.intercalate "\n" $
        [ "", "== Update summary ==" ]
        ++ map renderOutcome os
        ++ [ verdict os ]

verdict :: [UpdateOutcome] -> Text
verdict os =
    case [() | o <- os, isFailure (uoStatus o)] of
        [] -> "\nAll updates complete."
        fs -> "\nFAILED: " <> T.pack (show (length fs)) <> " update(s) had problems."
  where
    isFailure UpdateUnchanged          = False
    isFailure UpdateChanged            = False
    isFailure (UpdateRestartFailed _)  = True
    isFailure (UpdateFailed _)         = True
