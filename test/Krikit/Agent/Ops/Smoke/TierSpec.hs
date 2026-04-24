{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the pure 'Tier' helpers: names, keys, counts, verdict.
module Krikit.Agent.Ops.Smoke.TierSpec (spec) where

import           Test.Hspec

import           Krikit.Agent.Ops.Smoke.Tier
    ( Counts (..)
    , Tier (..)
    , allPassed
    , countResults
    , failWith
    , pass
    , skipWith
    , tierKey
    , tierName
    )

spec :: Spec
spec = do
    describe "tierName / tierKey" $ do
        it "covers every Tier (bounded enum)" $ do
            let ts = [minBound .. maxBound :: Tier]
            length ts `shouldBe` 11
            mapM_ (\t -> tierName t `shouldSatisfy` (not . null . show)) ts
            mapM_ (\t -> tierKey  t `shouldSatisfy` (not . null . show)) ts

        it "uses stable keys for the machine log" $ do
            tierKey TierServices `shouldBe` "services"
            tierKey TierTelegram `shouldBe` "telegram"

    describe "countResults" $ do
        it "counts pass / fail / skip" $ do
            let rs =
                    [ pass     TierOllama    10 []
                    , failWith TierSentry   20 "flake" []
                    , skipWith TierTelegram  0 "no creds" []
                    , pass     TierAudit     5 []
                    ]
            countResults rs `shouldBe` Counts { cPass = 2, cFail = 1, cSkip = 1 }

    describe "allPassed" $ do
        it "is true when no tier has Fail status" $ do
            let rs =
                    [ pass     TierOllama  5 []
                    , skipWith TierTelegram 0 "no creds" []
                    ]
            allPassed rs `shouldBe` True

        it "is false if any tier is Fail" $ do
            let rs =
                    [ pass     TierOllama  5 []
                    , failWith TierSentry  20 "flake" []
                    ]
            allPassed rs `shouldBe` False
