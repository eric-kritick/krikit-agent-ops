{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the repo-inventory cross-check verifier.
module Krikit.Agent.Ops.Verify.RepoInventorySpec (spec) where

import qualified Data.Set                              as Set
import qualified Data.Text                             as T
import           Test.Hspec

import           Krikit.Agent.Ops.Verify.Common        (Finding (..),
                                                        Severity (..))
import           Krikit.Agent.Ops.Verify.RepoInventory (buildFindings)

spec :: Spec
spec = do
    describe "buildFindings" $ do
        let noExceptions = Set.empty

        it "is clean when inventory and on-disk match exactly" $ do
            let inv = Set.fromList ["kritick-admin", "kritick-api"]
                fs  = Set.fromList ["kritick-admin", "kritick-api"]
            buildFindings inv noExceptions fs `shouldBe` []

        it "reports Error for inventory roots not cloned to disk" $ do
            let inv = Set.fromList ["kritick-admin", "kritick-api"]
                fs  = Set.fromList ["kritick-admin"]
                got = buildFindings inv noExceptions fs
            length got                  `shouldBe` 1
            map fSeverity got           `shouldBe` [Error]
            map fSubject got            `shouldBe` ["kritick-api"]

        it "reports Warning for kritick-* dirs unknown to the inventory" $ do
            let inv = Set.fromList ["kritick-admin"]
                fs  = Set.fromList ["kritick-admin", "kritick-stray"]
                got = buildFindings inv noExceptions fs
            length got                  `shouldBe` 1
            map fSeverity got           `shouldBe` [Warning]
            map fSubject got            `shouldBe` ["kritick-stray"]

        it "treats rootExceptions as intentional non-roots (no warning)" $ do
            -- kritick-ecosystem is the meta repo; lives on disk but
            -- is intentionally excluded from `ecosystem-roots.generated.md`
            -- via `auto_roots.exclude_repos`.
            let inv  = Set.fromList ["kritick-admin"]
                fs   = Set.fromList ["kritick-admin", "kritick-ecosystem"]
                excs = Set.fromList ["kritick-ecosystem"]
            buildFindings inv excs fs `shouldBe` []

        it "ignores non-kritick-* on-disk repos (out of scope)" $ do
            let inv = Set.fromList ["kritick-admin"]
                fs  = Set.fromList [ "kritick-admin"
                                   , "krikit-agent-ops"
                                   , "purescript-aws-s3-free"
                                   , "scala-mongodb-free"
                                   ]
            buildFindings inv noExceptions fs `shouldBe` []

        it "produces both errors and warnings together when both apply" $ do
            let inv = Set.fromList ["kritick-admin", "kritick-api"]
                fs  = Set.fromList ["kritick-admin", "kritick-stray"]
                got = buildFindings inv noExceptions fs
                sevs = map fSeverity got
            length got `shouldBe` 2
            Error   `elem` sevs `shouldBe` True
            Warning `elem` sevs `shouldBe` True

        it "error message names ecosystem-roots.generated.md" $ do
            let inv = Set.fromList ["kritick-admin", "kritick-missing"]
                fs  = Set.fromList ["kritick-admin"]
                got = buildFindings inv noExceptions fs
            case got of
                [Finding _ _ msg] ->
                    msg `shouldSatisfy` T.isInfixOf "ecosystem-roots.generated.md"
                _ -> expectationFailure "expected exactly one finding"

        it "warning message guides the operator to the right knob" $ do
            let inv = Set.fromList ["kritick-admin"]
                fs  = Set.fromList ["kritick-admin", "kritick-stray"]
                got = buildFindings inv noExceptions fs
            case got of
                [Finding _ _ msg] -> do
                    msg `shouldSatisfy` T.isInfixOf "ignore"
                    msg `shouldSatisfy` T.isInfixOf "ecosystem.json"
                _ -> expectationFailure "expected exactly one finding"
