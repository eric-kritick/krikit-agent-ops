{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Orchestrator for the @krikit-update-status@ binary.
--
-- For each 'Tool', query the installed version and (if applicable) the
-- upstream npm registry version, classify into 'UpdateAvailability',
-- and bundle into a 'ToolStatus' record. This module is
-- effect-polymorphic; the IO-bearing handlers live in the executable.
module Krikit.Agent.Ops.UpdateStatus.Run
    ( ToolStatus (..)
    , checkAllTools
    , checkTool
    , queryInstalledVersion
    , queryNpmLatest
    ) where

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Effectful                      (Eff, (:>))

import           Krikit.Agent.Ops.Effect.Proc
    ( Proc
    , ProcResult (..)
    , runCmd
    )
import           Krikit.Agent.Ops.Units         (Seconds (..))
import           Krikit.Agent.Ops.UpdateStatus.Tool
    ( Tool
    , UpstreamSource (..)
    , allTools
    , toolBinary
    , toolUpstream
    , toolVersionArgs
    )
import           Krikit.Agent.Ops.UpdateStatus.Version
    ( UpdateAvailability (..)
    , Version (..)
    , compareVersions
    , extractVersion
    )

-- | Per-tool report. Carries enough to feed a formatter without
-- requiring it to redo any classification.
data ToolStatus = ToolStatus
    { tsTool         :: !Tool
    , tsInstalled    :: !(Maybe Version)
    , tsAvailability :: !UpdateAvailability
    }
    deriving stock (Eq, Show)

-- | Standard timeout for any one subprocess call. Both @<tool>
-- --version@ and @npm show <pkg> version@ should return well under
-- this; longer values wait on a hung network.
checkTimeout :: Seconds
checkTimeout = Seconds 15

-- | Check every 'Tool' in the closed set.
checkAllTools :: (Proc :> es) => Eff es [ToolStatus]
checkAllTools = mapM checkTool allTools

-- | Probe a single tool: get its installed version, then determine
-- update availability based on the upstream source.
checkTool :: (Proc :> es) => Tool -> Eff es ToolStatus
checkTool tool = do
    installed <- queryInstalledVersion tool
    avail <- decideAvailability tool installed
    pure ToolStatus
        { tsTool         = tool
        , tsInstalled    = installed
        , tsAvailability = avail
        }

decideAvailability
    :: (Proc :> es)
    => Tool -> Maybe Version -> Eff es UpdateAvailability
decideAvailability tool installed =
    case toolUpstream tool of
        AutoUpdates ->
            -- Whether or not we got the installed version, there's no
            -- remote check to perform for an auto-updater.
            pure NoRemoteCheck

        NpmRegistry pkg -> do
            latest <- queryNpmLatest pkg
            pure (classify installed latest)
  where
    classify :: Maybe Version -> Maybe Version -> UpdateAvailability
    classify (Just iv) (Just lv) = compareVersions iv lv
    classify Nothing   (Just _)  = Unknown "couldn't determine installed version"
    classify (Just _)  Nothing   = Unknown "couldn't reach npm registry"
    classify Nothing   Nothing   = Unknown "neither installed nor upstream version known"

-- | Run @<binary> --version@ and pull a semver-like substring from the
-- output. Returns 'Nothing' on subprocess error or unparseable output.
queryInstalledVersion :: (Proc :> es) => Tool -> Eff es (Maybe Version)
queryInstalledVersion tool = do
    let bin = toolBinary tool
        args = toolVersionArgs tool
    r <- runCmd bin args checkTimeout
    pure $ case r of
        Right ProcResult { prStdout = out } -> extractVersion out
        Left _                              -> Nothing

-- | Run @npm show <pkg> version@ and parse its trimmed stdout as a
-- 'Version'. Returns 'Nothing' on subprocess error or empty output.
queryNpmLatest :: (Proc :> es) => Text -> Eff es (Maybe Version)
queryNpmLatest pkg = do
    r <- runCmd "npm" ["show", T.unpack pkg, "version"] checkTimeout
    pure $ case r of
        Right ProcResult { prStdout = out } ->
            let trimmed = T.strip out
            in  if T.null trimmed
                    then Nothing
                    else Just (Version trimmed)
        Left _ -> Nothing
