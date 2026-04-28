{-# LANGUAGE OverloadedStrings #-}

-- | krikit-versions: print installed CLI versions across the
-- agent stack, grouped by user account.
module Main (main) where

import qualified Data.Text.IO                 as TIO

import           Effectful                    (runEff)

import           Krikit.Agent.Ops.Effect.Proc (runProcIO)
import           Krikit.Agent.Ops.Versions.Run
    ( renderAll
    , runAllSections
    )

main :: IO ()
main = do
    sections <- runEff . runProcIO $ runAllSections
    TIO.putStrLn (renderAll sections)
