{-# LANGUAGE OverloadedStrings #-}

-- | Tests for repo classification + inventory building.
module Krikit.Agent.Ops.RepoInventory.RunSpec (spec) where

import qualified Data.Map.Strict                          as Map
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import           Test.Hspec

import           Krikit.Agent.Ops.RepoInventory.Run
    ( Category (..)
    , EcosystemFilter (..)
    , Inventory (..)
    , applyFilter
    , buildInventory
    , classify
    , extractEcosystemRoots
    , repoNote
    )

spec :: Spec
spec = do
    describe "classify" $ do
        it "routes coordination repos" $ do
            classify "kritick-ecosystem" `shouldBe` CatKritickCoordination
            classify "kritick-feature"   `shouldBe` CatKritickCoordination

        it "routes kritick product SPAs" $ do
            classify "kritick-admin"          `shouldBe` CatKritickProduct
            classify "kritick-customer"       `shouldBe` CatKritickProduct
            classify "kritick-manager"        `shouldBe` CatKritickProduct
            classify "kritick-representative" `shouldBe` CatKritickProduct

        it "routes other kritick-* to services" $ do
            classify "kritick-api"     `shouldBe` CatKritickService
            classify "kritick-billing" `shouldBe` CatKritickService

        it "routes krikit-* to ops infrastructure" $ do
            classify "krikit-agent-ops"    `shouldBe` CatKrikit
            classify "krikit-agent-fabric" `shouldBe` CatKrikit

        it "routes purescript-*-free regardless of suffix order" $ do
            classify "purescript-aws-s3-free" `shouldBe` CatPureScriptFree
            classify "purescript-mongodb-free" `shouldBe` CatPureScriptFree

        it "routes purescript-*-data" $
            classify "purescript-aws-s3-data" `shouldBe` CatPureScriptData

        it "routes purescript-react-* before generic purescript-*" $
            classify "purescript-react-bootstrap" `shouldBe` CatPureScriptReact

        it "routes purescript-kritick-* to kritick-domain" $
            classify "purescript-kritick-data" `shouldBe` CatPureScriptKritick

        it "routes scala-*-free / scala-*-data" $ do
            classify "scala-aws-s3-free" `shouldBe` CatScalaFree
            classify "scala-aws-s3-data" `shouldBe` CatScalaData

        it "routes ng-kritick-*" $
            classify "ng-kritick-foo" `shouldBe` CatNgKritick

        it "folds ng-safe-apply into ng-kritick category" $
            classify "ng-safe-apply" `shouldBe` CatNgKritick

        it "routes fastlane-ios-certificates as sensitive supporting" $
            classify "fastlane-ios-certificates"
                `shouldBe` CatKritickProductSensitive

        it "routes aws-bedrock-prompts as prompt assets" $
            classify "aws-bedrock-prompts" `shouldBe` CatPromptAssets

        it "routes autocoder as experimental" $
            classify "autocoder" `shouldBe` CatExperimental

        it "routes react-* / react-kritick-*" $ do
            classify "react-bootstrap-anchorless"   `shouldBe` CatReact
            classify "react-kritick-advanced-search" `shouldBe` CatReact

        it "routes util-* and template-*" $ do
            classify "util-github"        `shouldBe` CatUtil
            classify "template-purescript" `shouldBe` CatTemplate

        it "routes known build-tooling repos" $ do
            classify "aws-cloudformation-kritick" `shouldBe` CatBuildTooling
            classify "git-bower"                  `shouldBe` CatBuildTooling

        it "routes known root-level schema repos" $ do
            classify "mongodb-kritick-schema"      `shouldBe` CatSchemaRoots
            classify "elasticsearch-kritick-schema" `shouldBe` CatSchemaRoots

        it "falls back to Unknown for stray entries" $ do
            classify "unrelated-tool" `shouldBe` CatUnknown
            classify ""               `shouldBe` CatUnknown

    describe "buildInventory" $ do
        let inv = buildInventory
                [ "kritick-admin"
                , "kritick-api"
                , "kritick-admin"   -- dup
                , "krikit-agent-ops"
                , "purescript-aws-s3-free"
                , "totally-foreign"
                ]

        it "deduplicates entries" $
            Map.lookup CatKritickProduct (invByCategory inv)
                `shouldBe` Just ["kritick-admin"]

        it "places repos by their classification" $ do
            Map.lookup CatKritickService (invByCategory inv)
                `shouldBe` Just ["kritick-api"]
            Map.lookup CatKrikit (invByCategory inv)
                `shouldBe` Just ["krikit-agent-ops"]
            Map.lookup CatPureScriptFree (invByCategory inv)
                `shouldBe` Just ["purescript-aws-s3-free"]

        it "captures uncategorized stragglers" $
            Map.lookup CatUnknown (invByCategory inv)
                `shouldBe` Just ["totally-foreign"]

    describe "repoNote" $ do
        it "flags fastlane-ios-certificates as sensitive" $ do
            case repoNote "fastlane-ios-certificates" of
                Nothing   -> expectationFailure "expected a note"
                Just note -> note `shouldSatisfy` (T.isInfixOf "Sensitive")

        it "returns Nothing for ordinary repos" $ do
            repoNote "kritick-admin" `shouldBe` Nothing

    describe "applyFilter" $ do
        let f = EcosystemFilter
                  { efIgnoreNames    = Set.fromList ["docker", "rn-test"]
                  , efIgnorePrefixes = ["deprecated--"]
                  }

        it "drops repos by exact name" $
            applyFilter f ["docker", "kritick-admin", "rn-test"]
                `shouldBe` ["kritick-admin"]

        it "drops repos by prefix" $
            applyFilter f ["deprecated--foo", "kritick-admin"]
                `shouldBe` ["kritick-admin"]

        it "preserves order otherwise" $
            applyFilter f ["alpha", "beta", "docker", "gamma"]
                `shouldBe` ["alpha", "beta", "gamma"]

    describe "extractEcosystemRoots" $ do
        it "pulls the repo column out of an ecosystem-roots-shaped doc" $ do
            let doc = T.unlines
                    [ "# Ecosystem roots (generated)"
                    , ""
                    , "Root count: **3**"
                    , ""
                    , "| repo | in-degree |"
                    , "|---|---:|"
                    , "| kritick-admin    | 0 |"
                    , "| kritick-customer | 1 |"
                    , "| kritick-api      | 0 |"
                    ]
            extractEcosystemRoots doc
                `shouldBe` ["kritick-admin", "kritick-customer", "kritick-api"]

        it "returns an empty list when no table is present" $
            extractEcosystemRoots "no tables here\n" `shouldBe` []
