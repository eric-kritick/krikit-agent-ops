{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration-shaped tests for 'checkTool' using mock subprocess
-- handlers. Exercises the same effect-swap pattern the smoke suite
-- uses for tier checks.
module Krikit.Agent.Ops.UpdateStatus.RunSpec (spec) where

import qualified Data.Map.Strict                       as Map
import           Test.Hspec

import           Effectful                             (runPureEff)

import           Krikit.Agent.Ops.Effect.Proc
    ( mockResponse
    , runProcMock
    )
import           Krikit.Agent.Ops.Units                (Milliseconds (..))
import           Krikit.Agent.Ops.UpdateStatus.Run     (ToolStatus (..), checkTool)
import           Krikit.Agent.Ops.UpdateStatus.Tool    (Tool (..))
import           Krikit.Agent.Ops.UpdateStatus.Version
    ( UpdateAvailability (..)
    , Version (..)
    )

spec :: Spec
spec = do
    describe "checkTool ToolCodex (npm-backed)" $ do

        it "passes when installed matches npm's latest" $ do
            let mocks = Map.fromList
                    [ ("codex",
                       mockResponse "0.5.2\n" "" (Milliseconds 50))
                    , ("npm",
                       mockResponse "0.5.2\n" "" (Milliseconds 200))
                    ]
                ts = runPureEff $ runProcMock mocks (checkTool ToolCodex)
            tsAvailability ts `shouldBe` UpToDate
            tsInstalled    ts `shouldBe` Just (Version "0.5.2")

        it "flags an update when installed < latest" $ do
            let mocks = Map.fromList
                    [ ("codex",
                       mockResponse "0.5.2\n" "" (Milliseconds 50))
                    , ("npm",
                       mockResponse "0.6.0\n" "" (Milliseconds 200))
                    ]
                ts = runPureEff $ runProcMock mocks (checkTool ToolCodex)
            tsAvailability ts `shouldBe` UpdateAvailable (Version "0.6.0")
            tsInstalled    ts `shouldBe` Just (Version "0.5.2")

        it "reports Unknown when codex --version fails" $ do
            let mocks = Map.fromList
                    [ -- no entry for "codex" -> handler returns ProcLaunchErr
                      ("npm",
                       mockResponse "0.5.2\n" "" (Milliseconds 200))
                    ]
                ts = runPureEff $ runProcMock mocks (checkTool ToolCodex)
            case tsAvailability ts of
                Unknown _ -> pure ()
                other     -> expectationFailure
                                ("expected Unknown, got " <> show other)

        it "reports Unknown when npm registry is unreachable" $ do
            let mocks = Map.fromList
                    [ ("codex",
                       mockResponse "0.5.2\n" "" (Milliseconds 50))
                    -- no entry for "npm" -> ProcLaunchErr
                    ]
                ts = runPureEff $ runProcMock mocks (checkTool ToolCodex)
            tsInstalled ts `shouldBe` Just (Version "0.5.2")
            case tsAvailability ts of
                Unknown _ -> pure ()
                other     -> expectationFailure
                                ("expected Unknown, got " <> show other)

    describe "checkTool ToolClaude (auto-updates)" $ do

        it "reports NoRemoteCheck regardless of npm" $ do
            let mocks = Map.fromList
                    [ ("claude",
                       mockResponse "claude version 2.1.118 (Claude Code)\n"
                                    "" (Milliseconds 30))
                    -- npm absent on purpose; should never be invoked
                    ]
                ts = runPureEff $ runProcMock mocks (checkTool ToolClaude)
            tsAvailability ts `shouldBe` NoRemoteCheck
            tsInstalled    ts `shouldBe` Just (Version "2.1.118")
