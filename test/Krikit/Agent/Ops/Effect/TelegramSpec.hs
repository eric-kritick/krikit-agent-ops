{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the Telegram effect's mock + capture handlers.
-- The IO handler is exercised end-to-end by the on-mini deploy;
-- here we just confirm the abstraction shape and the capture
-- handler.
module Krikit.Agent.Ops.Effect.TelegramSpec (spec) where

import           Data.IORef                       (newIORef, readIORef)
import           Test.Hspec

import           Effectful                        (runEff)

import           Krikit.Agent.Ops.Effect.Telegram (BotToken (..),
                                                   MessageId (..), runTelegramCapture,
                                                   runTelegramMock,
                                                   sendMessage)

spec :: Spec
spec = do
    describe "runTelegramMock" $ do
        it "always returns Right (MessageId 0)" $ do
            r <- runEff . runTelegramMock $ sendMessage "hi"
            r `shouldBe` Right (MessageId 0)

    describe "runTelegramCapture" $ do
        it "appends sent messages in send order" $ do
            ref <- newIORef []
            _ <- runEff . runTelegramCapture ref $ do
                _ <- sendMessage "first"
                _ <- sendMessage "second"
                _ <- sendMessage "third"
                pure ()
            captured <- readIORef ref
            captured `shouldBe` ["first", "second", "third"]

    describe "BotToken Show" $ do
        it "redacts the token" $
            show (BotToken "ghp_secret123") `shouldBe` "BotToken <redacted>"
