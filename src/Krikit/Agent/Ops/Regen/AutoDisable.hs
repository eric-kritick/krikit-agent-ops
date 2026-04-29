{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Three-strikes auto-disable for cron-driven jobs.
--
-- A regen or verify job that fails three runs in a row gets
-- short-circuited on the next run -- launchd keeps re-firing
-- harmlessly, the digest surfaces the disabled state, and the
-- operator removes the marker file once the underlying cause is
-- fixed.
--
-- Without this, a broken job (e.g. malformed input the binary
-- can't parse) would generate a noisy alert every five minutes
-- and a noisy digest line every morning, indefinitely. With it,
-- the alarm fires once on each transition (clean -> failing,
-- failing -> disabled), then the digest carries a calm
-- @DISABLED@ status until the operator reverses it.
--
-- Storage layout under the configured state directory (defaults
-- to @\/var\/lib\/krikit\/regen\/@):
--
--   * @\<job\>.failures@ -- consecutive-failure counter (text
--     containing a single integer). Reset to 0 on a clean run.
--   * @\<job\>.disabled@ -- absence == enabled. Presence ==
--     disabled; file body is the timestamp + last-known exit
--     code, written when the counter crossed the threshold.
--
-- Exit code semantics across the regen + verify suite:
--
-- @
--   0 -- clean: the job did exactly what it should have
--   1 -- soft: warnings only, e.g. \"openclaw.json absent
--        off-mini\" -- expected, no concern. Counter reset.
--   2 -- hard: error, action required. Counter increments.
--   3 -- input failure: missing config / unreadable inputs.
--        Counter increments (it's a hard failure too).
-- @
--
-- Anything @\>= 2@ counts as a strike. Codes @0@ and @1@ reset
-- the counter -- a job that flickers between clean and warning
-- never auto-disables.
module Krikit.Agent.Ops.Regen.AutoDisable
    ( -- * Constants
      defaultStateDir
    , disableThreshold

      -- * Pure helpers
    , isStrike
    , advanceCounter

      -- * Effectful entry point
    , runWithAutoDisable

      -- * Inspection (used by RegenSummary)
    , JobStatus (..)
    , readJobStatus
    , disabledBodyForOperator
    ) where

import           Control.Exception   (IOException, try)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.Time           as Time
import           System.Directory    (createDirectoryIfMissing,
                                      doesFileExist, removeFile)
import           System.Exit         (ExitCode (..), exitWith)
import           System.FilePath     ((<.>), (</>))
import           Text.Read           (readMaybe)

-- =============================================================================
-- Constants
-- =============================================================================

-- | Where the per-job counter and disable marker files live.
defaultStateDir :: FilePath
defaultStateDir = "/var/lib/krikit/regen"

-- | Number of consecutive non-zero exits required before a job
-- auto-disables itself. Three is the canonical \"two might be
-- noise, three is a pattern\" rule.
disableThreshold :: Int
disableThreshold = 3

-- =============================================================================
-- Pure helpers
-- =============================================================================

-- | Does this exit code count toward the three-strikes counter?
-- See module-level docstring for the policy.
isStrike :: ExitCode -> Bool
isStrike = \case
    ExitSuccess     -> False
    ExitFailure n   -> n >= 2

-- | Pure counter update: previous-counter + new-exit-code ->
-- (new-counter, just-crossed-threshold). When the counter
-- reaches 'disableThreshold' the second component is 'True', and
-- callers should write the disable marker.
advanceCounter :: Int -> ExitCode -> (Int, Bool)
advanceCounter prev ec
    | not (isStrike ec) = (0, False)
    | otherwise         =
        let new = prev + 1
            crossed = prev < disableThreshold && new >= disableThreshold
        in  (new, crossed)

-- =============================================================================
-- Effectful entry point
-- =============================================================================

-- | Wrap a job's main action with the three-strikes machinery.
-- The action returns its own 'ExitCode' (do NOT call 'exitWith'
-- inside it -- this wrapper handles the exit). Behaviour:
--
--   1. If @\<stateDir\>\/\<jobName\>.disabled@ exists, print a
--      one-line notice on stdout, exit 0. Launchd respawns
--      harmlessly; the operator's daily digest surfaces the
--      disabled status.
--   2. Otherwise run the action, capture its 'ExitCode'.
--   3. Update the counter. If 'isStrike', increment; else reset
--      to 0.
--   4. If the counter crossed 'disableThreshold' on this run,
--      write the disable marker.
--   5. 'exitWith' the original code.
--
-- Failures inside the wrapper itself (counter file unreadable,
-- state dir not writable) are swallowed -- the wrapper never
-- masks the action's exit code with its own bookkeeping
-- failures.
runWithAutoDisable
    :: Text         -- ^ job name (e.g. @\"regen-system-state-mini\"@)
    -> FilePath     -- ^ state directory ('defaultStateDir' on the mini)
    -> IO ExitCode  -- ^ the actual job
    -> IO a
runWithAutoDisable jobName stateDir action = do
    let counterPath  = stateDir </> T.unpack jobName <.> "failures"
        disabledPath = stateDir </> T.unpack jobName <.> "disabled"

    disabled <- doesFileExist disabledPath
    if disabled
        then do
            body <- readSafe disabledPath
            TIO.putStrLn $
                "DISABLED: " <> jobName
                <> " (three strikes; "
                <> firstLine body
                <> ")"
            exitWith ExitSuccess
        else do
            ec <- action

            prev <- readCounter counterPath
            let (next, crossed) = advanceCounter prev ec
            _ <- try @IOException
                (createDirectoryIfMissing True stateDir
                 >> writeCounter counterPath next)

            -- Reset wins if not a strike: nuke any stale marker.
            -- (Belt-and-suspenders; if we got here with a clean
            -- exit, by definition there's no .disabled in scope.)
            case (isStrike ec, crossed) of
                (False, _) -> tryRemove disabledPath
                (True, True) -> do
                    nowText <- isoUtcNow
                    let body = nowText
                            <> " — last exit: "
                            <> exitCodeText ec
                            <> "; counter reached "
                            <> T.pack (show next)
                            <> " (threshold "
                            <> T.pack (show disableThreshold)
                            <> ")"
                    _ <- try @IOException (TIO.writeFile disabledPath body)
                    pure ()
                (True, False) -> pure ()

            exitWith ec

-- =============================================================================
-- Inspection (RegenSummary uses these)
-- =============================================================================

-- | What the digest needs to know about a job, beyond its log.
data JobStatus
    = JobEnabled              -- ^ no marker; counter content irrelevant
    | JobDisabled !Text       -- ^ marker present; body of marker file
    deriving stock (Eq, Show)

-- | Read a job's auto-disable status from disk. Missing marker
-- yields 'JobEnabled'.
readJobStatus :: FilePath -> Text -> IO JobStatus
readJobStatus stateDir jobName = do
    let p = stateDir </> T.unpack jobName <.> "disabled"
    exists <- doesFileExist p
    if not exists
        then pure JobEnabled
        else JobDisabled . firstLine <$> readSafe p

-- | One-line excerpt of the marker body, suitable for Telegram-
-- style summaries.
disabledBodyForOperator :: Text -> Text
disabledBodyForOperator = capLength 200 . firstLine

-- =============================================================================
-- Internal helpers
-- =============================================================================

readSafe :: FilePath -> IO Text
readSafe p = do
    r <- try @IOException (TIO.readFile p)
    case r of
        Right t -> pure t
        Left _  -> pure ""

readCounter :: FilePath -> IO Int
readCounter p = do
    body <- readSafe p
    pure $ case readMaybe (T.unpack (T.strip body)) of
        Just n | n >= 0 -> n
        _               -> 0

writeCounter :: FilePath -> Int -> IO ()
writeCounter p n = TIO.writeFile p (T.pack (show n) <> "\n")

tryRemove :: FilePath -> IO ()
tryRemove p = do
    exists <- doesFileExist p
    if exists
        then do
            _ <- try @IOException (removeFile p)
            pure ()
        else pure ()

firstLine :: Text -> Text
firstLine t = case T.lines t of
    []      -> ""
    (l : _) -> l

capLength :: Int -> Text -> Text
capLength n t
    | T.length t <= n = t
    | otherwise       = T.take n t <> "…"

exitCodeText :: ExitCode -> Text
exitCodeText = \case
    ExitSuccess     -> "0"
    ExitFailure n   -> T.pack (show n)

isoUtcNow :: IO Text
isoUtcNow = do
    now <- Time.getCurrentTime
    pure (T.pack
            (Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now))
