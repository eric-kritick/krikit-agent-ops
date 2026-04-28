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
    , readMacOSStatus
    , parseMacOSCache

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

-- | Outcome of inspecting the macOS update cache.
data MacOSStatus
    = MacOSUpToDate          !Int
    -- ^ No pending updates. Carries the cache age in days.
    | MacOSUpdatesAvailable  !Int !Int
    -- ^ N updates pending. Carries (count, age in days).
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
        MacOSUpdatesAvailable (countPendingLines body) ageDays
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

countPendingLines :: Text -> Int
countPendingLines = length . filter startsWithStar . T.lines

startsWithStar :: Text -> Bool
startsWithStar = ("* " `T.isPrefixOf`)
