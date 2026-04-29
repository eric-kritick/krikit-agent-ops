{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Idempotent file writes for regenerators.
--
-- A generator that always rewrites its output causes spurious git
-- diffs on every cron run (the wall-clock @Last generated@ line
-- changes even when nothing else did). This module compares the
-- proposed content against the existing file, ignoring the timestamp
-- line, and only writes when the substantive content has actually
-- changed. The on-disk wall-clock line stays put across no-op runs.
module Krikit.Agent.Ops.Regen.Write
    ( -- * Domain types
      WriteOutcome (..)

      -- * Operations
    , writeIfChanged
    , renderOutcome
    ) where

import           Control.Exception                 (IOException, try)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as TIO
import           System.Directory                  (createDirectoryIfMissing, doesFileExist)
import           System.FilePath                   (takeDirectory)

import           Krikit.Agent.Ops.Regen.Banner     (normalizeForCompare)

-- | What 'writeIfChanged' did.
data WriteOutcome
    = Created     -- ^ file did not exist; we wrote it for the first time
    | Written     -- ^ file existed; substantive content changed; we rewrote it
    | Unchanged   -- ^ file existed; substantive content matched; left untouched
    | WriteError !Text
                  -- ^ I/O failed (permission, disk, etc.)
    deriving stock (Eq, Show)

-- | Write @new@ to @path@ only if its normalized form differs from
-- the existing file's normalized form. \"Normalized\" strips the
-- @Last generated: ...@ banner line so a date-only delta does not
-- count as a change.
writeIfChanged :: FilePath -> Text -> IO WriteOutcome
writeIfChanged path new = do
    exists <- doesFileExist path
    if not exists
        then attemptWrite Created
        else do
            old <- TIO.readFile path
            if normalizeForCompare old == normalizeForCompare new
                then pure Unchanged
                else attemptWrite Written
  where
    attemptWrite ok = do
        let dir = takeDirectory path
        result <- try $ do
            createDirectoryIfMissing True dir
            TIO.writeFile path new
        case result of
            Right ()                    -> pure ok
            Left (e :: IOException)     -> pure (WriteError (T.pack (show e)))

-- | Human-readable one-liner suitable for a generator's stdout.
renderOutcome :: FilePath -> WriteOutcome -> Text
renderOutcome path = \case
    Created          -> "  CREATED:    " <> T.pack path
    Written          -> "  WRITTEN:    " <> T.pack path
    Unchanged        -> "  UNCHANGED:  " <> T.pack path
    WriteError msg   -> "  WRITE-FAIL: " <> T.pack path <> " -- " <> msg
