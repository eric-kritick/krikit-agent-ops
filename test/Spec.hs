module Main (main) where

import qualified Krikit.Agent.Ops.Smoke.RunSpec
import qualified Krikit.Agent.Ops.Smoke.TierSpec
import           Test.Hspec                    (hspec)

main :: IO ()
main = hspec $ do
    Krikit.Agent.Ops.Smoke.TierSpec.spec
    Krikit.Agent.Ops.Smoke.RunSpec.spec
