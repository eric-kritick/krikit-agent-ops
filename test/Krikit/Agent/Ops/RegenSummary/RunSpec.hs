{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the regen-summary parsers + renderer.
module Krikit.Agent.Ops.RegenSummary.RunSpec (spec) where

import qualified Data.Text                              as T
import           Test.Hspec

import           Krikit.Agent.Ops.RegenSummary.Run
    ( RegenChange (..)
    , RegenJob (..)
    , RegenStatus (..)
    , VerifyJob (..)
    , VerifyStatus (..)
    , parseRegenLog
    , parseVerifyLog
    , parseWorkspaceSyncLog
    , renderRegenLine
    , renderVerifyLine
    , renderSummary
    )

spec :: Spec
spec = do
    describe "parseRegenLog" $ do
        it "recognizes UNCHANGED" $
            parseRegenLog "  UNCHANGED:  /tmp/x.md\n"
                `shouldBe` RegenOk Unchanged

        it "recognizes WRITTEN" $
            parseRegenLog "  WRITTEN:    /tmp/x.md\n"
                `shouldBe` RegenOk Written

        it "recognizes CREATED" $
            parseRegenLog "  CREATED:    /tmp/x.md\n"
                `shouldBe` RegenOk Created

        it "recognizes WRITE-FAIL with reason" $
            parseRegenLog "  WRITE-FAIL: /tmp/x.md -- permission denied\n"
                `shouldBe` RegenWriteFail "/tmp/x.md -- permission denied"

        it "picks the LAST matching line in a multi-run log" $ do
            -- A log that accumulated across two cron runs: yesterday
            -- WRITTEN, today UNCHANGED. We want today's status.
            let body = T.unlines
                    [ "  WRITTEN:    /tmp/x.md"
                    , "  UNCHANGED:  /tmp/x.md"
                    ]
            parseRegenLog body `shouldBe` RegenOk Unchanged

        it "is unparseable when neither marker appears" $
            case parseRegenLog "FAIL: something unrelated\n" of
                RegenUnparseable _ -> pure ()
                other              ->
                    expectationFailure ("expected RegenUnparseable, got "
                                        <> show other)

    describe "parseWorkspaceSyncLog" $ do
        let summaryOk =
                "{\"ts\":\"2026-04-30T06:05:01Z\",\"event\":\"summary\",\
                \\"repo_count\":290,\"ok\":290,\"failed\":0,\
                \\"failed_repos\":[],\"total_ms\":312541}"
            summaryFail =
                "{\"ts\":\"2026-04-30T06:05:01Z\",\"event\":\"summary\",\
                \\"repo_count\":290,\"ok\":287,\"failed\":3,\
                \\"failed_repos\":[\"a\",\"b\",\"c\"],\"total_ms\":312541}"

        it "parses an all-ok summary" $
            parseWorkspaceSyncLog summaryOk
                `shouldBe` RegenSyncOk 290 0

        it "parses a summary with failures" $
            parseWorkspaceSyncLog summaryFail
                `shouldBe` RegenSyncFail 287 3

        it "ignores fetch-event lines, picks the summary line" $ do
            let body = T.unlines
                    [ "{\"event\":\"start\",\"repo_count\":2}"
                    , "{\"event\":\"fetch\",\"repo\":\"a\",\"status\":\"ok\"}"
                    , "{\"event\":\"fetch\",\"repo\":\"b\",\"status\":\"ok\"}"
                    , summaryOk
                    ]
            parseWorkspaceSyncLog body `shouldBe` RegenSyncOk 290 0

    describe "parseVerifyLog" $ do
        it "recognizes a clean verifier run" $ do
            let body = T.unlines
                    [ "# krikit-verify-x"
                    , ""
                    , "  CLEAN: no findings."
                    , ""
                    , "Summary: 0 findings (0 errors, 0 warnings)."
                    ]
            parseVerifyLog body `shouldBe` VerifyClean

        it "recognizes warnings-only" $ do
            let body = "Summary: 3 findings (0 errors, 3 warnings).\n"
            parseVerifyLog body `shouldBe` VerifyWarn 3

        it "recognizes errors (with warnings count)" $ do
            let body = "Summary: 5 findings (2 errors, 3 warnings).\n"
            parseVerifyLog body `shouldBe` VerifyErr 2 3

        it "is unparseable without a Summary footer" $
            case parseVerifyLog "Just some prose, no footer.\n" of
                VerifyUnparseable _ -> pure ()
                other               ->
                    expectationFailure ("expected VerifyUnparseable, got "
                                        <> show other)

    describe "renderRegenLine" $ do
        let job n = RegenJob n (T.unpack n <> ".log")

        it "best case: all ok, all unchanged" $ do
            let pairs =
                    [ (job "workspace-sync",        RegenSyncOk 290 0)
                    , (job "system-state-mini",    RegenOk Unchanged)
                    , (job "repo-inventory",        RegenOk Unchanged)
                    , (job "cross-reference-index", RegenOk Unchanged)
                    ]
            renderRegenLine pairs
                `shouldBe` "Regen: 4/4 ok (all unchanged)"

        it "names a written output" $ do
            let pairs =
                    [ (job "workspace-sync",        RegenSyncOk 290 0)
                    , (job "system-state-mini",    RegenOk Unchanged)
                    , (job "repo-inventory",        RegenOk Written)
                    , (job "cross-reference-index", RegenOk Unchanged)
                    ]
            renderRegenLine pairs
                `shouldBe` "Regen: 4/4 ok (1 written: repo-inventory)"

        it "names failures and writes when both happen" $ do
            let pairs =
                    [ (job "workspace-sync",        RegenSyncFail 287 3)
                    , (job "system-state-mini",    RegenOk Unchanged)
                    , (job "repo-inventory",        RegenOk Written)
                    , (job "cross-reference-index", RegenLogMissing)
                    ]
            renderRegenLine pairs
                `shouldBe` "Regen: 2/4 ok (2 failed: workspace-sync, cross-reference-index; 1 written: repo-inventory)"

    describe "renderVerifyLine" $ do
        let job n = VerifyJob n (T.unpack n <> ".log")

        it "all clean" $ do
            let pairs =
                    [ (job "reading-order",  VerifyClean)
                    , (job "llm-channel",    VerifyClean)
                    , (job "repo-inventory", VerifyClean)
                    ]
            renderVerifyLine pairs `shouldBe` "Verify: 3/3 clean"

        it "warnings only -- no name suffix" $ do
            let pairs =
                    [ (job "reading-order",  VerifyClean)
                    , (job "llm-channel",    VerifyWarn 3)
                    , (job "repo-inventory", VerifyClean)
                    ]
            renderVerifyLine pairs
                `shouldBe` "Verify: 2 clean, 1 warn"

        it "errors get an err: <names> suffix" $ do
            let pairs =
                    [ (job "reading-order",  VerifyClean)
                    , (job "llm-channel",    VerifyWarn 3)
                    , (job "repo-inventory", VerifyErr 1 0)
                    ]
            renderVerifyLine pairs
                `shouldBe` "Verify: 1 clean, 1 warn, 1 err (err: repo-inventory)"

    describe "renderSummary" $ do
        it "emits exactly two lines separated by \\n" $ do
            let txt = renderSummary [] []
            length (T.lines txt) `shouldBe` 2
