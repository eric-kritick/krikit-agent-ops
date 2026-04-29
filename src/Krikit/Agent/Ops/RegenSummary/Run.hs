{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | @krikit-regen-summary@: produce a two-line summary of last
-- night's regen + verify chain status, suitable for inclusion in
-- the daily Telegram digest (PB 10).
--
-- Reads the per-job log files under
-- @\/var\/log\/krikit\/regen\/@. Output:
--
-- @
-- Regen: 4\/4 ok (1 written: repo-inventory)
-- Verify: 2 clean, 1 warn (llm-channel)
-- @
--
-- Or, when nothing's notable:
--
-- @
-- Regen: 4\/4 ok (all unchanged)
-- Verify: 3\/3 clean
-- @
--
-- When a job has been auto-disabled (three strikes, see
-- 'Krikit.Agent.Ops.Regen.AutoDisable'), it's named in the
-- summary so the operator notices in the daily digest:
--
-- @
-- Regen: 3\/4 ok (1 disabled: system-state-mini)
-- Verify: 2\/3 clean (1 disabled: llm-channel)
-- @
--
-- Pure parsing + rendering live here; the orchestrator just
-- reads files and dispatches.
module Krikit.Agent.Ops.RegenSummary.Run
    ( -- * Domain types
      RegenChange (..)
    , RegenStatus (..)
    , VerifyStatus (..)
    , RegenJob (..)
    , VerifyJob (..)

      -- * Pure parsers
    , parseRegenLog
    , parseVerifyLog
    , parseWorkspaceSyncLog

      -- * Pure rendering
    , renderSummary
    , renderRegenLine
    , renderVerifyLine

      -- * Effectful orchestrator
    , summarize
    , defaultLogDir
    , defaultRegenJobs
    , defaultVerifyJobs
    ) where

import           Control.Exception                       (IOException, try)
import           Data.Maybe                              (mapMaybe)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import qualified Data.Text.IO                            as TIO
import           System.Directory                        (doesFileExist)
import           System.FilePath                         ((</>))
import           Text.Read                               (readMaybe)

import           Krikit.Agent.Ops.Regen.AutoDisable
    ( JobStatus (..)
    , readJobStatus
    )

-- =============================================================================
-- Domain types
-- =============================================================================

-- | What happened to the file a regen job writes.
data RegenChange
    = Unchanged       -- ^ existing content matched; file untouched
    | Written         -- ^ existing content differed; rewritten
    | Created         -- ^ file did not exist; first-time write
    deriving stock (Eq, Show)

-- | One regen job's outcome. @RegenOk@ also captures
-- workspace-sync's @(ok, failed)@ pair when that's relevant.
data RegenStatus
    = RegenOk RegenChange
    | RegenWriteFail !Text
    | RegenSyncOk !Int !Int  -- ^ workspace-sync: (ok-count, failed-count)
    | RegenSyncFail !Int !Int
      -- ^ workspace-sync with failures > 0
    | RegenLogMissing
    | RegenUnparseable !Text
    | RegenDisabled !Text
      -- ^ auto-disable marker present; payload is the marker body
      --   (timestamp + last-known exit code + counter info).
    deriving stock (Eq, Show)

-- | One verify job's outcome.
data VerifyStatus
    = VerifyClean
    | VerifyWarn !Int
    | VerifyErr !Int !Int  -- ^ (errors, warnings)
    | VerifyLogMissing
    | VerifyUnparseable !Text
    | VerifyDisabled !Text
      -- ^ auto-disable marker present.
    deriving stock (Eq, Show)

-- | One regen job. The @rjLogFile@ is a base name within the log
-- directory. @rjAutoDisableName@ is the identifier registered with
-- 'Krikit.Agent.Ops.Regen.AutoDisable.runWithAutoDisable' (i.e.
-- the @\<name\>@ in @\<name\>.disabled@); 'Nothing' means the job
-- isn't wrapped (e.g. @workspace-sync@, which is a Python script).
data RegenJob = RegenJob
    { rjName             :: !Text
    , rjLogFile          :: !FilePath
    , rjAutoDisableName  :: !(Maybe Text)
    }
    deriving stock (Eq, Show)

-- | One verify job. Same shape.
data VerifyJob = VerifyJob
    { vjName             :: !Text
    , vjLogFile          :: !FilePath
    , vjAutoDisableName  :: !(Maybe Text)
    }
    deriving stock (Eq, Show)

-- =============================================================================
-- Pure parsers
-- =============================================================================

-- | Parse a regen-binary's log. The binary's last meaningful
-- line is one of @Created@\/@Written@\/@Unchanged@\/@WriteError@
-- per 'Krikit.Agent.Ops.Regen.Write.renderOutcome'. We look for
-- those tokens line-by-line, taking the last hit (in case the
-- log accumulated across multiple cron runs without rotation).
parseRegenLog :: Text -> RegenStatus
parseRegenLog body =
    case lastMatching match (T.lines body) of
        Just s  -> s
        Nothing -> RegenUnparseable (firstNonEmptyLine body)
  where
    match :: Text -> Maybe RegenStatus
    match line =
        let s = T.stripStart line
        in  if      "CREATED:"    `T.isPrefixOf` s then Just (RegenOk Created)
            else if "WRITTEN:"    `T.isPrefixOf` s then Just (RegenOk Written)
            else if "UNCHANGED:"  `T.isPrefixOf` s then Just (RegenOk Unchanged)
            else if "WRITE-FAIL:" `T.isPrefixOf` s
                then Just (RegenWriteFail (T.strip (T.drop (T.length "WRITE-FAIL:") s)))
            else Nothing

-- | Parse @workspace_sync.py@'s JSON-lines log. We pick out the
-- final @{"event":"summary",...}@ line and read @ok@ \/ @failed@
-- counts. JSON parsing is intentionally crude (substring grabs)
-- so we don't add an aeson dep just for two integers.
parseWorkspaceSyncLog :: Text -> RegenStatus
parseWorkspaceSyncLog body =
    case lastMatching summaryLine (T.lines body) of
        Just s  -> s
        Nothing -> RegenUnparseable (firstNonEmptyLine body)
  where
    summaryLine :: Text -> Maybe RegenStatus
    summaryLine line
        | not ("\"event\":\"summary\"" `T.isInfixOf` line) = Nothing
        | otherwise =
            case (extractInt "\"ok\":" line, extractInt "\"failed\":" line) of
                (Just ok, Just fl)
                    | fl > 0    -> Just (RegenSyncFail ok fl)
                    | otherwise -> Just (RegenSyncOk ok fl)
                _               -> Just (RegenUnparseable line)

    extractInt :: Text -> Text -> Maybe Int
    extractInt key line =
        case T.breakOn key line of
            (_, rest) | T.null rest -> Nothing
            (_, rest) ->
                let after = T.drop (T.length key) rest
                    digits = T.takeWhile (\c -> c >= '0' && c <= '9') after
                in  if T.null digits then Nothing
                    else readMaybe (T.unpack digits)

-- | Parse a verify binary's log. The binary emits one
-- @Summary: N findings (E errors, W warnings).@ footer per
-- 'Krikit.Agent.Ops.Verify.Common.renderFindings'.
parseVerifyLog :: Text -> VerifyStatus
parseVerifyLog body =
    case lastMatching summary (T.lines body) of
        Just s  -> s
        Nothing -> VerifyUnparseable (firstNonEmptyLine body)
  where
    summary :: Text -> Maybe VerifyStatus
    summary line
        | not ("Summary:" `T.isPrefixOf` T.stripStart line) = Nothing
        | otherwise =
            case (extractInt "errors" line, extractInt "warnings" line) of
                (Just e, Just w)
                    | e > 0  -> Just (VerifyErr e w)
                    | w > 0  -> Just (VerifyWarn w)
                    | otherwise -> Just VerifyClean
                _ -> Just (VerifyUnparseable line)

    -- "(N errors" / "M warnings" are the canonical shapes.
    extractInt :: Text -> Text -> Maybe Int
    extractInt suffix line =
        case T.breakOn suffix line of
            (_, rest) | T.null rest -> Nothing
            (before, _) ->
                -- read trailing digits from the part before the keyword
                let trimmed = T.dropWhileEnd (== ' ') before
                    revDigs = T.takeWhileEnd (\c -> c >= '0' && c <= '9') trimmed
                in  if T.null revDigs then Nothing
                    else readMaybe (T.unpack revDigs)

-- =============================================================================
-- Pure rendering
-- =============================================================================

-- | Render the full two-line summary.
renderSummary :: [(RegenJob, RegenStatus)] -> [(VerifyJob, VerifyStatus)] -> Text
renderSummary regens verifies =
    renderRegenLine regens <> "\n" <> renderVerifyLine verifies

-- | Line 1 -- regen status.
renderRegenLine :: [(RegenJob, RegenStatus)] -> Text
renderRegenLine [] = "Regen: (no jobs)"
renderRegenLine pairs =
    let total      = length pairs
        oks        = filter (isOk        . snd) pairs
        writtens   = filter (isWritten   . snd) pairs
        faileds    = filter (isFailed    . snd) pairs
        disableds  = filter (isDisabled  . snd) pairs
        nOk        = length oks
        nWritten   = length writtens
        nFailed    = length faileds
        nDisabled  = length disableds

        head_      = "Regen: "
                  <> T.pack (show nOk)
                  <> "/"
                  <> T.pack (show total)
                  <> " ok"

        details
            | nFailed == 0 && nWritten == 0 && nDisabled == 0 =
                " (all unchanged)"
            | otherwise =
                let writtenPart =
                        if nWritten == 0 then []
                        else [ T.pack (show nWritten) <> " written: "
                            <> T.intercalate ", " (map (rjName . fst) writtens) ]
                    failedPart =
                        if nFailed == 0 then []
                        else [ T.pack (show nFailed) <> " failed: "
                            <> T.intercalate ", " (map (rjName . fst) faileds) ]
                    disabledPart =
                        if nDisabled == 0 then []
                        else [ T.pack (show nDisabled) <> " disabled: "
                            <> T.intercalate ", " (map (rjName . fst) disableds) ]
                    parts = disabledPart ++ failedPart ++ writtenPart
                in  " (" <> T.intercalate "; " parts <> ")"
    in  head_ <> details
  where
    -- DISABLED is *not* "ok" -- the regen didn't run. It also
    -- isn't a strike-bearing failure -- the wrapper exited 0.
    -- So it lives in its own slot.
    isOk = \case
        RegenOk _       -> True
        RegenSyncOk _ _ -> True
        _               -> False

    isWritten = \case
        RegenOk Written  -> True
        RegenOk Created  -> True
        _                -> False

    isFailed = \case
        RegenWriteFail _   -> True
        RegenSyncFail _ _  -> True
        RegenLogMissing    -> True
        RegenUnparseable _ -> True
        _                  -> False

    isDisabled = \case
        RegenDisabled _ -> True
        _               -> False

-- | Line 2 -- verify status.
renderVerifyLine :: [(VerifyJob, VerifyStatus)] -> Text
renderVerifyLine [] = "Verify: (no jobs)"
renderVerifyLine pairs =
    let total = length pairs
        cleans   = filter (isClean    . snd) pairs
        warns    = filter (isWarn     . snd) pairs
        errs     = filter (isErr      . snd) pairs
        unks     = filter (isUnknown  . snd) pairs
        disableds = filter (isDisabled . snd) pairs
        nClean    = length cleans
        nWarn     = length warns
        nErr      = length errs
        nUnk      = length unks
        nDisabled = length disableds

        allClean = nClean == total

        body
            | allClean =
                T.pack (show nClean) <> "/" <> T.pack (show total) <> " clean"
            | otherwise =
                let parts =
                        [ T.pack (show nClean) <> " clean" | nClean > 0 ]
                     ++ [ T.pack (show nWarn)  <> " warn"  | nWarn  > 0 ]
                     ++ [ T.pack (show nErr)   <> " err"   | nErr   > 0 ]
                     ++ [ T.pack (show nDisabled) <> " disabled" | nDisabled > 0 ]
                     ++ [ T.pack (show nUnk)   <> " unknown" | nUnk > 0 ]
                in  T.intercalate ", " parts

        suffixParts =
            (if nErr > 0
                then ["err: " <> T.intercalate ", " (map (vjName . fst) errs)]
                else [])
            ++ (if nDisabled > 0
                then ["disabled: " <> T.intercalate ", " (map (vjName . fst) disableds)]
                else [])

        suffix
            | null suffixParts = ""
            | otherwise        =
                " (" <> T.intercalate "; " suffixParts <> ")"
    in  "Verify: " <> body <> suffix
  where
    isClean = \case VerifyClean    -> True; _ -> False
    isWarn  = \case VerifyWarn _   -> True; _ -> False
    isErr   = \case VerifyErr _ _  -> True; _ -> False
    isUnknown = \case
        VerifyLogMissing    -> True
        VerifyUnparseable _ -> True
        _                   -> False
    isDisabled = \case
        VerifyDisabled _ -> True
        _                -> False

-- =============================================================================
-- Effectful orchestrator
-- =============================================================================

-- | Default log directory on the mini.
defaultLogDir :: FilePath
defaultLogDir = "/var/log/krikit/regen"

-- | The default set of regen jobs we summarize. workspace-sync
-- has a special parser; the rest share the regenerator
-- @WRITTEN\/UNCHANGED\/CREATED\/WRITE-FAIL@ shape. The
-- 'rjAutoDisableName' values match the labels each binary passes
-- to 'Krikit.Agent.Ops.Regen.AutoDisable.runWithAutoDisable'.
defaultRegenJobs :: [RegenJob]
defaultRegenJobs =
    [ RegenJob "workspace-sync"
        "workspace-sync.log"            Nothing
    , RegenJob "system-state-mini"
        "regen-system-state-mini.log"
        (Just "regen-system-state-mini")
    , RegenJob "repo-inventory"
        "regen-repo-inventory.log"
        (Just "regen-repo-inventory")
    , RegenJob "cross-reference-index"
        "regen-cross-reference-index.log"
        (Just "regen-cross-reference-index")
    ]

defaultVerifyJobs :: [VerifyJob]
defaultVerifyJobs =
    [ VerifyJob "reading-order"
        "verify-reading-order.log"
        (Just "verify-reading-order")
    , VerifyJob "llm-channel"
        "verify-llm-channel-consistency.log"
        (Just "verify-llm-channel-consistency")
    , VerifyJob "repo-inventory"
        "verify-repo-inventory.log"
        (Just "verify-repo-inventory")
    ]

-- | Read every job's log under @logDir@ and produce the rendered
-- two-line summary. Missing or unparseable logs become
-- 'RegenLogMissing' \/ 'RegenUnparseable' (or the verify
-- equivalents) so the digest still ships -- a missing log is a
-- visible signal, not a hard failure.
--
-- Auto-disable markers under @stateDir@ (see
-- 'Krikit.Agent.Ops.Regen.AutoDisable') take precedence: a
-- disabled job's log content is irrelevant, so we surface the
-- disabled state directly.
summarize :: FilePath -> FilePath -> IO Text
summarize logDir stateDir = do
    rs <- mapM (readRegenJob  logDir stateDir) defaultRegenJobs
    vs <- mapM (readVerifyJob logDir stateDir) defaultVerifyJobs
    pure (renderSummary rs vs)

readRegenJob :: FilePath -> FilePath -> RegenJob -> IO (RegenJob, RegenStatus)
readRegenJob logDir stateDir job = do
    disabled <- jobDisabled stateDir (rjAutoDisableName job)
    case disabled of
        Just body -> pure (job, RegenDisabled body)
        Nothing   -> readRegenLog logDir job

readRegenLog :: FilePath -> RegenJob -> IO (RegenJob, RegenStatus)
readRegenLog logDir job = do
    let path = logDir </> rjLogFile job
    exists <- doesFileExist path
    if not exists
        then pure (job, RegenLogMissing)
        else do
            r <- try (TIO.readFile path)
            case r of
                Left (_ :: IOException) ->
                    pure (job, RegenLogMissing)
                Right body
                    | rjName job == "workspace-sync" ->
                        pure (job, parseWorkspaceSyncLog body)
                    | otherwise ->
                        pure (job, parseRegenLog body)

readVerifyJob
    :: FilePath -> FilePath -> VerifyJob -> IO (VerifyJob, VerifyStatus)
readVerifyJob logDir stateDir job = do
    disabled <- jobDisabled stateDir (vjAutoDisableName job)
    case disabled of
        Just body -> pure (job, VerifyDisabled body)
        Nothing   -> readVerifyLog logDir job

readVerifyLog :: FilePath -> VerifyJob -> IO (VerifyJob, VerifyStatus)
readVerifyLog logDir job = do
    let path = logDir </> vjLogFile job
    exists <- doesFileExist path
    if not exists
        then pure (job, VerifyLogMissing)
        else do
            r <- try (TIO.readFile path)
            case r of
                Left (_ :: IOException) ->
                    pure (job, VerifyLogMissing)
                Right body ->
                    pure (job, parseVerifyLog body)

-- | Resolve a job's auto-disable marker, if any. A 'Nothing'
-- identifier means the job isn't wrapped (e.g. @workspace-sync@).
jobDisabled :: FilePath -> Maybe Text -> IO (Maybe Text)
jobDisabled _        Nothing     = pure Nothing
jobDisabled stateDir (Just name) = do
    s <- readJobStatus stateDir name
    pure $ case s of
        JobEnabled       -> Nothing
        JobDisabled body -> Just body

-- =============================================================================
-- Helpers
-- =============================================================================

-- | Find the *last* element for which the predicate yields
-- 'Just'. We want the latest matching log line in case the file
-- accumulated multiple cron runs without rotation.
lastMatching :: (a -> Maybe b) -> [a] -> Maybe b
lastMatching p xs =
    case mapMaybe p xs of
        []  -> Nothing
        ys  -> Just (last ys)

firstNonEmptyLine :: Text -> Text
firstNonEmptyLine body =
    case dropWhile T.null (map T.strip (T.lines body)) of
        []      -> ""
        (x : _) -> T.take 80 x  -- cap to avoid huge stderrs in the digest
