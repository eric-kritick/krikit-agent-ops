{-# LANGUAGE OverloadedStrings #-}

-- | krikit-health: one-shot multi-section health report.
module Main (main) where

import qualified Data.Text.IO                   as TIO

import           Effectful                      (runEff)

import           Krikit.Agent.Ops.Effect.Probe  (defaultManager, runProbeIO)
import           Krikit.Agent.Ops.Effect.Proc   (runProcIO)
import           Krikit.Agent.Ops.Health.Run    (renderSections, runHealth)

main :: IO ()
main = do
    mgr <- defaultManager
    sections <- runEff
                  . runProcIO
                  . runProbeIO mgr
                  $ runHealth
    TIO.putStrLn (renderSections sections)
