{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the shared verify scaffolding.
module Krikit.Agent.Ops.Verify.CommonSpec (spec) where

import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import           System.Exit                       (ExitCode (..))
import           Test.Hspec

import           Krikit.Agent.Ops.Verify.Common
    ( Finding (..)
    , SetDiff (..)
    , Severity (..)
    , compareSets
    , exitForFindings
    , maxSeverity
    , renderFindings
    )

spec :: Spec
spec = do
    describe "compareSets" $ do
        it "partitions into left-only / right-only / both" $ do
            let l = Set.fromList ["a", "b", "c"] :: Set.Set T.Text
                r = Set.fromList ["b", "c", "d"] :: Set.Set T.Text
                d = compareSets l r
            sdLeftOnly  d `shouldBe` ["a"]
            sdRightOnly d `shouldBe` ["d"]
            sdBoth      d `shouldBe` ["b", "c"]

        it "sorts each output bucket" $ do
            let l = Set.fromList ["zebra", "apple", "mango"] :: Set.Set T.Text
                r = Set.fromList ["banana"] :: Set.Set T.Text
            sdLeftOnly (compareSets l r)
                `shouldBe` ["apple", "mango", "zebra"]

        it "is correct on empty inputs" $ do
            let d = compareSets Set.empty (Set.empty :: Set.Set Int)
            sdLeftOnly  d `shouldBe` []
            sdRightOnly d `shouldBe` []
            sdBoth      d `shouldBe` []

    describe "maxSeverity" $ do
        it "is Info on an empty list" $
            maxSeverity [] `shouldBe` Info

        it "returns the highest present" $ do
            let mk s = Finding s "x" "y"
            maxSeverity [mk Info, mk Warning]              `shouldBe` Warning
            maxSeverity [mk Info, mk Warning, mk Error]    `shouldBe` Error
            maxSeverity [mk Error, mk Warning]             `shouldBe` Error

    describe "exitForFindings" $ do
        let mk s = Finding s "x" "y"
        it "exits 0 on a clean run" $
            exitForFindings [] `shouldBe` ExitSuccess

        it "exits 0 on Info-only" $
            exitForFindings [mk Info, mk Info] `shouldBe` ExitSuccess

        it "exits 1 on Warning-only" $
            exitForFindings [mk Info, mk Warning] `shouldBe` ExitFailure 1

        it "exits 2 when any Error is present" $ do
            exitForFindings [mk Error] `shouldBe` ExitFailure 2
            exitForFindings [mk Warning, mk Error] `shouldBe` ExitFailure 2

    describe "renderFindings" $ do
        it "renders a clean run with the verifier name" $ do
            let txt = renderFindings "krikit-verify-foo" []
            txt `shouldSatisfy` T.isInfixOf "krikit-verify-foo"
            txt `shouldSatisfy` T.isInfixOf "CLEAN"

        it "labels each finding by severity" $ do
            let fs = [ Finding Error   "kritick-foo"  "missing"
                     , Finding Warning "stray-thing"  "unexpected"
                     ]
                txt = renderFindings "krikit-verify-foo" fs
            txt `shouldSatisfy` T.isInfixOf "[ERR ]"
            txt `shouldSatisfy` T.isInfixOf "[WARN]"
            txt `shouldSatisfy` T.isInfixOf "kritick-foo"
            txt `shouldSatisfy` T.isInfixOf "stray-thing"

        it "summary footer reports counts by severity" $ do
            let fs = [ Finding Error   "a" "x"
                     , Finding Error   "b" "y"
                     , Finding Warning "c" "z"
                     ]
                txt = renderFindings "krikit-verify-foo" fs
            txt `shouldSatisfy` T.isInfixOf "2 errors"
            txt `shouldSatisfy` T.isInfixOf "1 warnings"
