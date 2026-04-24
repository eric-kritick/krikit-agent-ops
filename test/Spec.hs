module Main (main) where

import qualified Krikit.Version as V
import           System.Exit (exitFailure)

main :: IO ()
main = do
    putStrLn "krikit-agent-ops test suite"
    if V.versionString == "0.1.0.0"
        then putStrLn "  OK: version matches"
        else do
            putStrLn "  FAIL: version mismatch"
            exitFailure
