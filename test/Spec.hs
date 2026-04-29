module Main (main) where

import qualified Krikit.Agent.Ops.ConfigSpec
import qualified Krikit.Agent.Ops.CrossReferenceIndex.RunSpec
import qualified Krikit.Agent.Ops.Regen.MarkdownExtractSpec
import qualified Krikit.Agent.Ops.Regen.WriteSpec
import qualified Krikit.Agent.Ops.RepoInventory.RunSpec
import qualified Krikit.Agent.Ops.Verify.CommonSpec
import qualified Krikit.Agent.Ops.Verify.LlmChannelConsistencySpec
import qualified Krikit.Agent.Ops.Verify.ReadingOrderSpec
import qualified Krikit.Agent.Ops.Verify.RepoInventorySpec
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
    Krikit.Agent.Ops.Regen.MarkdownExtractSpec.spec
    Krikit.Agent.Ops.Regen.WriteSpec.spec
    Krikit.Agent.Ops.RepoInventory.RunSpec.spec
    Krikit.Agent.Ops.ConfigSpec.spec
    Krikit.Agent.Ops.CrossReferenceIndex.RunSpec.spec
    Krikit.Agent.Ops.Verify.CommonSpec.spec
    Krikit.Agent.Ops.Verify.LlmChannelConsistencySpec.spec
    Krikit.Agent.Ops.Verify.ReadingOrderSpec.spec
    Krikit.Agent.Ops.Verify.RepoInventorySpec.spec
