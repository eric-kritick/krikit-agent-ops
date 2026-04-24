module Main (main) where

import qualified Krikit.Version as V

main :: IO ()
main =
    putStrLn $ "krikit-smoke v" ++ V.versionString ++ " -- scaffold OK"
