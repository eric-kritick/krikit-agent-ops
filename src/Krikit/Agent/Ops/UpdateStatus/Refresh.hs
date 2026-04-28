{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | macOS update cache refresh logic.
--
-- Calls @softwareupdate -l@ (slow; ~30s) and writes the output to
-- the cache file used by the read path in
-- "Krikit.Agent.Ops.UpdateStatus.MacOS". Replaces the old
-- @krikit-macos-updates@ bash script.
--
-- Requires root because the cache lives under @/var/lib/krikit/@,
-- which is owned by root. The CLI surfaces a clear error if not
-- run with sudo.
module Krikit.Agent.Ops.UpdateStatus.Refresh
    ( RefreshOutcome (..)
    , refreshCache
    ) where

import           Control.Exception              (IOException, try)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO

import           Effectful                      (Eff, IOE, liftIO, (:>))

import           Krikit.Agent.Ops.Effect.Proc
    ( Proc
    , ProcError (..)
    , ProcResult (..)
    , runCmd
    )
import           Krikit.Agent.Ops.Units         (Seconds (..))
import           Krikit.Agent.Ops.UpdateStatus.MacOS
    ( MacOSStatus (..)
    , defaultMacOSCachePath
    , parseMacOSCache
    )

-- | Result of a refresh attempt. Carries the post-refresh status so
-- the caller can render a one-line summary without re-reading the
-- file.
data RefreshOutcome
    = RefreshOk         !MacOSStatus
    -- ^ @softwareupdate -l@ ran and the cache was written.
    | RefreshNeedsRoot
    -- ^ Cache write failed with EACCES; user needs to re-run with sudo.
    | RefreshSubprocessFailed !Text
    -- ^ @softwareupdate -l@ itself errored; payload is the reason.
    | RefreshWriteFailed      !Text
    -- ^ Subprocess succeeded but writing the cache file errored.
    deriving stock (Eq, Show)

-- | Run @softwareupdate -l@, write the output to the cache file,
-- best-effort chown to @agentops:admin@, and parse the result so
-- the caller can render a summary.
refreshCache
    :: (Proc :> es, IOE :> es)
    => Eff es RefreshOutcome
refreshCache = do
    liftIO $ TIO.putStrLn
        "Querying Apple's update server (may take ~30s)..."

    r <- runCmd "softwareupdate" ["-l"] (Seconds 90)
    case r of
        Left e ->
            pure (RefreshSubprocessFailed (procErrorMsg e))

        Right ProcResult { prStdout = out, prStderr = err } -> do
            -- softwareupdate writes its content to stderr; combine
            -- both so we don't lose anything on different macOS
            -- versions.
            let body = err <> out

            writeResult <- liftIO $ try @IOException
                (TIO.writeFile defaultMacOSCachePath body)

            case writeResult of
                Left e
                    | isPermissionDeniedMessage (T.pack (show e))
                        -> pure RefreshNeedsRoot
                    | otherwise
                        -> pure (RefreshWriteFailed (T.pack (show e)))

                Right () -> do
                    -- Best-effort chown so monitor.py (running as
                    -- agentops) keeps writing to the same file later
                    -- if it ever needs to. Failure is fine — we
                    -- already have the content in place.
                    _ <- runCmd "chown"
                            ["agentops:admin", defaultMacOSCachePath]
                            (Seconds 5)

                    -- Classify so the caller can summarize. Cache age
                    -- is "now" since we just wrote it.
                    pure (RefreshOk (parseMacOSCache body 0))

isPermissionDeniedMessage :: Text -> Bool
isPermissionDeniedMessage msg =
    "permission denied" `T.isInfixOf` T.toLower msg
        || "operation not permitted" `T.isInfixOf` T.toLower msg

procErrorMsg :: ProcError -> Text
procErrorMsg = \case
    ProcTimeout       -> "softwareupdate -l timed out"
    ProcNonZero n     -> "softwareupdate -l exit " <> T.pack (show n)
    ProcLaunchErr msg -> "couldn't launch softwareupdate: " <> msg
