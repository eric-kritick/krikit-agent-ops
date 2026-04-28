module Main (main) where

import qualified Krikit.Agent.Ops.Smoke.RunSpec
import qualified Krikit.Agent.Ops.Smoke.TierSpec
import qualified Krikit.Agent.Ops.UpdateStatus.MacOSSpec
import qualified Krikit.Agent.Ops.UpdateStatus.RunSpec
import qualified Krikit.Agent.Ops.UpdateStatus.VersionSpec
import           Test.Hspec                                  (hspec)

main :: IO ()
main = hspec $ do
    Krikit.Agent.Ops.Smoke.TierSpec.spec
    Krikit.Agent.Ops.Smoke.RunSpec.spec
    Krikit.Agent.Ops.UpdateStatus.VersionSpec.spec
    Krikit.Agent.Ops.UpdateStatus.RunSpec.spec
    Krikit.Agent.Ops.UpdateStatus.MacOSSpec.spec
