{-# LANGUAGE OverloadedStrings #-}

-- | krikit-update-status: print pending-update info for the agent
-- stack tools (codex, openclaw, claude) and macOS.
--
-- Output is plain text, suitable for either a terminal session or
-- embedding into the daily Telegram digest by the PB 10 monitor.
-- No CLI flags in v1; if/when we add a JSON output mode, optparse-
-- applicative arrives here.
module Main (main) where

import qualified Data.Text.IO                          as TIO

import           Effectful                             (runEff)

import           Krikit.Agent.Ops.Effect.Proc          (runProcIO)
import           Krikit.Agent.Ops.UpdateStatus.MacOS   (readMacOSStatus)
import           Krikit.Agent.Ops.UpdateStatus.Report  (renderReport)
import           Krikit.Agent.Ops.UpdateStatus.Run     (checkAllTools)

main :: IO ()
main = do
    macos <- readMacOSStatus
    tools <- runEff . runProcIO $ checkAllTools
    TIO.putStrLn (renderReport tools macos)
