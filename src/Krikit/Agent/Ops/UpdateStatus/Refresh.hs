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
    = RefreshOk               !MacOSStatus
    -- ^ @softwareupdate -l@ ran and the cache was written.
    | RefreshSubprocessFailed !Text
    -- ^ @softwareupdate -l@ itself errored; payload is the reason.
    | RefreshWriteFailed      !Text
    -- ^ Subprocess succeeded but writing the cache file errored.
    -- The most likely cause is stale ownership on the cache file
    -- (e.g. created by an earlier sudo-run version of the script);
    -- one-time fix:
    --
    -- @
    -- sudo chown agentops:admin /var/lib/krikit/macos-updates.cache
    -- @
    deriving stock (Eq, Show)

-- | Run @softwareupdate -l@ and write the output to the cache.
--
-- Doesn't need root: @softwareupdate -l@ is a read-only network call
-- anyone can make, and @\/var\/lib\/krikit\/@ is set up by PB 10
-- Phase 2 to be owned by @agentops:admin@ so agentops (which is the
-- user @monitor.py@ runs as) can write the cache directly.
--
-- The previous bash krikit-macos-updates used sudo for both steps;
-- the sudo was defensive, not required.
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
                Left e ->
                    pure (RefreshWriteFailed (T.pack (show e)))

                Right () ->
                    -- Cache age is "now" since we just wrote it.
                    pure (RefreshOk (parseMacOSCache body 0))

procErrorMsg :: ProcError -> Text
procErrorMsg = \case
    ProcTimeout       -> "softwareupdate -l timed out"
    ProcNonZero n     -> "softwareupdate -l exit " <> T.pack (show n)
    ProcLaunchErr msg -> "couldn't launch softwareupdate: " <> msg
