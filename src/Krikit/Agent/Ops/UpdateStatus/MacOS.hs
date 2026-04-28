{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | macOS pending-updates reporting based on the @krikit-macos-updates@
-- cache file written by Playbook 19.
--
-- The cache is the raw output of @softwareupdate -l@ (one shot) plus a
-- file mtime to age the result. We classify into a small ADT so the
-- formatter doesn't have to redo the parsing.
module Krikit.Agent.Ops.UpdateStatus.MacOS
    ( MacOSStatus (..)
    , MacOSUpdate (..)
    , readMacOSStatus
    , parseMacOSCache
    , extractUpdates

      -- * Defaults
    , defaultMacOSCachePath
    , defaultStaleThresholdDays
    ) where

import           Control.Exception  (IOException, try)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import qualified Data.Time          as Time
import           System.Directory   (doesFileExist, getModificationTime)

-- | A single pending update, parsed from one entry in the cache.
data MacOSUpdate = MacOSUpdate
    { muLabel :: !Text
    -- ^ The exact label, suitable for @softwareupdate -i \<label\>@.
    , muTitle :: !Text
    -- ^ Human-readable title, e.g. @"macOS Sequoia 15.5"@. May be
    -- empty if the cache entry didn't include a Title line.
    }
    deriving stock (Eq, Show)

-- | Outcome of inspecting the macOS update cache.
data MacOSStatus
    = MacOSUpToDate          !Int
    -- ^ No pending updates. Carries the cache age in days.
    | MacOSUpdatesAvailable  ![MacOSUpdate] !Int
    -- ^ Pending updates with their labels + titles, plus cache age.
    | MacOSCacheStale        !MacOSStatus
    -- ^ Wraps another status when the cache is older than the
    -- "stale" threshold; lets the formatter both show the result
    -- AND nag the operator to refresh.
    | MacOSCacheMissing
    -- ^ Cache file doesn't exist; @krikit-macos-updates@ never ran.
    | MacOSCacheUnparseable  !Text
    -- ^ File exists but doesn't match either expected shape; payload
    -- is a short reason for the report.
    deriving stock (Eq, Show)

defaultMacOSCachePath :: FilePath
defaultMacOSCachePath = "/var/lib/krikit/macos-updates.cache"

-- | After this many days, the cache is considered stale and the
-- operator should re-run @krikit-macos-updates@.
defaultStaleThresholdDays :: Int
defaultStaleThresholdDays = 7

-- | Read the cache from its default path and classify.
readMacOSStatus :: IO MacOSStatus
readMacOSStatus = readMacOSStatusAt defaultMacOSCachePath

readMacOSStatusAt :: FilePath -> IO MacOSStatus
readMacOSStatusAt path = do
    exists <- doesFileExist path
    if not exists
        then pure MacOSCacheMissing
        else do
            attempt <- try @IOException (TIO.readFile path)
            case attempt of
                Left e  ->
                    pure (MacOSCacheUnparseable
                            ("read error: " <> T.pack (show e)))
                Right body -> do
                    mt  <- getModificationTime path
                    now <- Time.getCurrentTime
                    let ageSec  = Time.diffUTCTime now mt
                        ageDays = floor (ageSec / 86400) :: Int
                        base    = parseMacOSCache body ageDays
                    pure (applyStaleness ageDays base)

-- | Pure helper: classify cache contents given a known age. Exposed
-- separately so tests can drive it without touching the filesystem.
parseMacOSCache :: Text -> Int -> MacOSStatus
parseMacOSCache body ageDays
    | "No new software available" `T.isInfixOf` body =
        MacOSUpToDate ageDays
    | hasPendingMarker body =
        MacOSUpdatesAvailable (extractUpdates body) ageDays
    | otherwise =
        MacOSCacheUnparseable
            "neither 'No new software available' nor '* '-prefixed lines found"

applyStaleness :: Int -> MacOSStatus -> MacOSStatus
applyStaleness ageDays s
    | ageDays > defaultStaleThresholdDays = MacOSCacheStale s
    | otherwise                           = s

-- | A "pending update" line in @softwareupdate -l@'s output starts with
-- @* @ at column 0.
hasPendingMarker :: Text -> Bool
hasPendingMarker = any startsWithStar . T.lines

startsWithStar :: Text -> Bool
startsWithStar = ("* " `T.isPrefixOf`)

-- | Walk the cache lines pulling out (label, title) pairs.
--
-- @softwareupdate -l@ emits each pending update as two lines:
--
-- @
-- * Label: macOS Sequoia 15.5-23F79
--   Title: macOS Sequoia 15.5, Version: 15.5, Size: 14.2 GiB, Recommended: YES, Action: restart,
-- @
--
-- We pair them up: every @* Label:@ line starts a new entry, and the
-- next line containing a @Title:@ field (before the next label, if
-- any) provides the title. Title is taken up to the first comma so
-- the report doesn't get cluttered with version/size suffixes.
--
-- An entry without a recognized Title line still counts; we emit it
-- with an empty 'muTitle' so the operator at least sees the label.
extractUpdates :: Text -> [MacOSUpdate]
extractUpdates = collect Nothing . T.lines
  where
    collect :: Maybe Text -> [Text] -> [MacOSUpdate]
    collect mLabel [] =
        case mLabel of
            Just l  -> [MacOSUpdate l ""]
            Nothing -> []
    collect mLabel (line : rest) =
        case T.stripPrefix "* Label: " (T.stripStart line) of
            Just newLabel ->
                let buffered = case mLabel of
                                 Just l  -> [MacOSUpdate l ""]
                                 Nothing -> []
                in  buffered ++ collect (Just (T.strip newLabel)) rest
            Nothing ->
                case (mLabel, extractTitle line) of
                    (Just l, Just t) ->
                        MacOSUpdate l t : collect Nothing rest
                    _ ->
                        collect mLabel rest

extractTitle :: Text -> Maybe Text
extractTitle line =
    case T.stripPrefix "Title:" (T.stripStart line) of
        Just rest -> Just (T.takeWhile (/= ',') (T.strip rest))
        Nothing   -> Nothing
