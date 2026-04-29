{-# LANGUAGE OverloadedStrings #-}

-- | Tests for repo classification + inventory building.
module Krikit.Agent.Ops.RepoInventory.RunSpec (spec) where

import qualified Data.List.NonEmpty                       as NE
import qualified Data.Map.Strict                          as Map
import qualified Data.Set                                 as Set
import qualified Data.Text                                as T
import           Data.Text                                (Text)
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

-- | Convenience: does @name@ classify under @cat@ in any of its
-- (potentially multiple) categories?
isIn :: Text -> Category -> Bool
isIn name cat = cat `elem` NE.toList (classify name)

spec :: Spec
spec = do
    describe "classify (single-category cases)" $ do
        it "routes coordination repos" $
            "kritick-ecosystem" `isIn` CatKritickCoordination `shouldBe` True

        it "routes pure-product SPAs" $ do
            "kritick-customer"       `isIn` CatKritickProduct `shouldBe` True
            "kritick-manager"        `isIn` CatKritickProduct `shouldBe` True
            "kritick-representative" `isIn` CatKritickProduct `shouldBe` True
            "kritick-blog"           `isIn` CatKritickProduct `shouldBe` True
            "kritick-help"           `isIn` CatKritickProduct `shouldBe` True
            "kritick-public"         `isIn` CatKritickProduct `shouldBe` True
            "kritick-session"        `isIn` CatKritickProduct `shouldBe` True

        it "routes other kritick-* to services (incl. kritick-feature)" $ do
            "kritick-api"     `isIn` CatKritickService `shouldBe` True
            "kritick-feature" `isIn` CatKritickService `shouldBe` True
            "kritick-billing" `isIn` CatKritickService `shouldBe` True

        it "routes krikit-* to ops infrastructure" $ do
            "krikit-agent-ops"    `isIn` CatKrikit `shouldBe` True
            "krikit-agent-fabric" `isIn` CatKrikit `shouldBe` True

        it "routes purescript-*-free regardless of suffix order" $
            "purescript-aws-s3-free" `isIn` CatPureScriptFree `shouldBe` True

        it "routes purescript-*-data" $
            "purescript-aws-s3-data" `isIn` CatPureScriptData `shouldBe` True

        it "routes purescript-react-* (web)" $
            "purescript-react-bootstrap" `isIn` CatPureScriptReact `shouldBe` True

        it "routes purescript-react-native-* (and the bare base) to its own bucket" $ do
            "purescript-react-native"          `isIn` CatPureScriptReactNative `shouldBe` True
            "purescript-react-native-firebase" `isIn` CatPureScriptReactNative `shouldBe` True

        it "catches purescript-*-react suffix (cloudinary-react, flexbox-react)" $ do
            "purescript-cloudinary-react" `isIn` CatPureScriptReact `shouldBe` True
            "purescript-flexbox-react"    `isIn` CatPureScriptReact `shouldBe` True

        it "routes purescript-kritick-*-data" $
            "purescript-kritick-admin-data" `isIn` CatPureScriptKritickData `shouldBe` True

        it "routes purescript-kritick-react-* to kritick React bucket" $
            "purescript-kritick-react-button" `isIn` CatPureScriptKritickReact `shouldBe` True

        it "routes other purescript-kritick-* (programs / dateutil / templates)" $ do
            "purescript-kritick-aws-programs"    `isIn` CatPureScriptKritickOther `shouldBe` True
            "purescript-kritick-dateutil"        `isIn` CatPureScriptKritickOther `shouldBe` True
            "purescript-kritick-email-templates" `isIn` CatPureScriptKritickOther `shouldBe` True

        it "routes scala-*-free / scala-*-data / scala-kritick-*" $ do
            "purescript-mongodb-free"  `isIn` CatPureScriptFree `shouldBe` True
            "scala-aws-s3-free"        `isIn` CatScalaFree `shouldBe` True
            "scala-aws-s3-data"        `isIn` CatScalaData `shouldBe` True
            "scala-kritick-data"       `isIn` CatScalaKritick `shouldBe` True
            "scala-kritick-free"       `isIn` CatScalaKritick `shouldBe` True
            "scala-kritick-hexastore"  `isIn` CatScalaKritick `shouldBe` True

        it "routes ng-kritick-* and ng-safe-apply" $ do
            "ng-kritick-foo" `isIn` CatNgKritick `shouldBe` True
            "ng-safe-apply"  `isIn` CatNgKritick `shouldBe` True

        it "routes react-* / react-kritick-*" $ do
            "react-bootstrap-anchorless"      `isIn` CatReact `shouldBe` True
            "react-kritick-advanced-search"   `isIn` CatReact `shouldBe` True

        it "routes util-* and template-*" $ do
            "util-github"        `isIn` CatUtil `shouldBe` True
            "template-purescript" `isIn` CatTemplate `shouldBe` True

        it "routes the schema family to CatKritickSchema" $ do
            "util-kritick-schema"                  `isIn` CatKritickSchema `shouldBe` True
            "purescript-kritick-schema"            `isIn` CatKritickSchema `shouldBe` True
            "purescript-kritick-key-schema"        `isIn` CatKritickSchema `shouldBe` True
            "purescript-kritick-feature-schema"    `isIn` CatKritickSchema `shouldBe` True
            "purescript-kritick-collection-schema" `isIn` CatKritickSchema `shouldBe` True
            "scala-kritick-schema"                 `isIn` CatKritickSchema `shouldBe` True
            "scala-kritick-key-schema"             `isIn` CatKritickSchema `shouldBe` True
            "scala-kritick-feature-schema"         `isIn` CatKritickSchema `shouldBe` True
            "scala-kritick-collection-schema"      `isIn` CatKritickSchema `shouldBe` True

        it "routes mongodb / redis schema repos to database infra" $ do
            "mongodb-kritick-schema" `isIn` CatDatabaseInfra `shouldBe` True
            "redis-kritick-schema"   `isIn` CatDatabaseInfra `shouldBe` True

        it "routes package management" $ do
            "purescript-package-sets" `isIn` CatPackageManagement `shouldBe` True
            "scala-maven-artifacts"   `isIn` CatPackageManagement `shouldBe` True

        it "routes non-code asset repos" $ do
            "aws-bedrock-prompts"   `isIn` CatAssetsNonCode `shouldBe` True
            "static-assets-kritick" `isIn` CatAssetsNonCode `shouldBe` True
            "static-legal-kritick"  `isIn` CatAssetsNonCode `shouldBe` True

        it "routes fastlane-ios-certificates as sensitive supporting" $
            "fastlane-ios-certificates" `isIn` CatKritickProductSensitive `shouldBe` True

        it "routes autocoder as experimental" $
            "autocoder" `isIn` CatExperimental `shouldBe` True

        it "routes known build-tooling repos (without static-assets / static-legal)" $ do
            "aws-cloudformation-kritick" `isIn` CatBuildTooling `shouldBe` True
            "git-bower"                  `isIn` CatBuildTooling `shouldBe` True

        it "routes the PureScript codecs / standards / date / geo / id substructure" $ do
            "purescript-base64-string"        `isIn` CatPureScriptCodecs       `shouldBe` True
            "purescript-foreign-xml"          `isIn` CatPureScriptCodecs       `shouldBe` True
            "purescript-iso3166"              `isIn` CatPureScriptStandards    `shouldBe` True
            "purescript-iso8601"              `isIn` CatPureScriptStandards    `shouldBe` True
            "purescript-dateformat"           `isIn` CatPureScriptDateTime     `shouldBe` True
            "purescript-unix-time"            `isIn` CatPureScriptDateTime     `shouldBe` True
            "purescript-geographic-coordinate" `isIn` CatPureScriptGeographic   `shouldBe` True
            "purescript-uuidstring"           `isIn` CatPureScriptIdentifiers  `shouldBe` True
            "purescript-jwtstring"            `isIn` CatPureScriptIdentifiers  `shouldBe` True

        it "leaves residual purescript-* in CatPureScriptOther" $ do
            "purescript-log"                  `isIn` CatPureScriptOther `shouldBe` True
            "purescript-formfield"            `isIn` CatPureScriptOther `shouldBe` True
            "purescript-aws-lambda-runtime"   `isIn` CatPureScriptOther `shouldBe` True

        it "falls back to Unknown for stray entries" $
            "totally-foreign" `isIn` CatUnknown `shouldBe` True

    describe "classify (multi-category cases)" $ do
        it "kritick-admin lists in BOTH product and service" $ do
            "kritick-admin" `isIn` CatKritickProduct `shouldBe` True
            "kritick-admin" `isIn` CatKritickService `shouldBe` True

        it "elasticsearch-kritick-schema lists in BOTH database infra and schema" $ do
            "elasticsearch-kritick-schema" `isIn` CatDatabaseInfra  `shouldBe` True
            "elasticsearch-kritick-schema" `isIn` CatKritickSchema  `shouldBe` True

    describe "buildInventory" $ do
        let inv = buildInventory
                [ "kritick-admin"
                , "kritick-api"
                , "kritick-admin"   -- dup
                , "krikit-agent-ops"
                , "purescript-aws-s3-free"
                , "elasticsearch-kritick-schema"
                , "totally-foreign"
                ]

        it "deduplicates entries even when multi-categorized" $ do
            -- kritick-admin appears in BOTH CatKritickProduct and CatKritickService
            Map.lookup CatKritickProduct (invByCategory inv)
                `shouldBe` Just ["kritick-admin"]
            Map.lookup CatKritickService (invByCategory inv)
                `shouldBe` Just ["kritick-admin", "kritick-api"]

        it "places multi-category repos in every applicable category" $ do
            Map.lookup CatDatabaseInfra (invByCategory inv)
                `shouldBe` Just ["elasticsearch-kritick-schema"]
            Map.lookup CatKritickSchema (invByCategory inv)
                `shouldBe` Just ["elasticsearch-kritick-schema"]

        it "captures uncategorized stragglers" $
            Map.lookup CatUnknown (invByCategory inv)
                `shouldBe` Just ["totally-foreign"]

    describe "repoNote" $ do
        it "flags fastlane-ios-certificates as sensitive" $ do
            case repoNote "fastlane-ios-certificates" of
                Nothing   -> expectationFailure "expected a note"
                Just note -> note `shouldSatisfy` (T.isInfixOf "Sensitive")

        it "flags kritick-admin as combined SPA + backend" $ do
            case repoNote "kritick-admin" of
                Nothing   -> expectationFailure "expected a note"
                Just note -> note `shouldSatisfy` (T.isInfixOf "SPA")

        it "flags kritick-customer as mobile" $ do
            case repoNote "kritick-customer" of
                Nothing   -> expectationFailure "expected a note"
                Just note -> note `shouldSatisfy` (T.isInfixOf "Mobile")

        it "flags schema-family entries with provenance" $ do
            case repoNote "scala-kritick-schema" of
                Just note -> note `shouldSatisfy` (T.isInfixOf "Autogenerated")
                Nothing   -> expectationFailure "expected a note"
            case repoNote "purescript-kritick-collection-schema" of
                Just note -> note `shouldSatisfy` (T.isInfixOf "Hardcoded")
                Nothing   -> expectationFailure "expected a note"

        it "flags email-templates with its kritick-email coupling" $ do
            case repoNote "purescript-kritick-email-templates" of
                Just note -> note `shouldSatisfy` (T.isInfixOf "kritick-email")
                Nothing   -> expectationFailure "expected a note"

        it "returns Nothing for ordinary repos" $
            repoNote "kritick-api" `shouldBe` Nothing

    describe "applyFilter" $ do
        let f = EcosystemFilter
                  { efIgnoreNames    = Set.fromList ["docker", "rn-test"]
                  , efIgnorePrefixes = ["deprecated--"]
                  , efRootExceptions = Set.empty
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
