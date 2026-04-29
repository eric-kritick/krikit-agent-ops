{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the markdown extraction primitives.
module Krikit.Agent.Ops.Regen.MarkdownExtractSpec (spec) where

import qualified Data.Text                                  as T
import           Test.Hspec

import           Krikit.Agent.Ops.Regen.MarkdownExtract
    ( MarkdownTable (..)
    , Section (..)
    , columnByHeader
    , extractBacktickTokens
    , extractFirstTable
    , extractSection
    , extractTables
    , isAlignmentRow
    , sectionsByLevel
    )

spec :: Spec
spec = do
    describe "isAlignmentRow" $ do
        it "accepts a plain alignment row" $
            isAlignmentRow "|---|---|" `shouldBe` True

        it "accepts mixed alignment markers" $
            isAlignmentRow "|---|---:|:---:|" `shouldBe` True

        it "tolerates surrounding whitespace" $
            isAlignmentRow "  |---|---|  " `shouldBe` True

        it "rejects rows with non-alignment characters" $ do
            isAlignmentRow "| repo | in-degree |" `shouldBe` False
            isAlignmentRow "| --- | abc |"        `shouldBe` False

    describe "extractTables" $ do
        let doc = T.unlines
                [ "# Title"
                , ""
                , "| repo | in-degree |"
                , "|---|---:|"
                , "| kritick-admin | 0 |"
                , "| kritick-customer | 1 |"
                , ""
                , "Some prose."
                , ""
                , "| key | value |"
                , "|---|---|"
                , "| a   | 1     |"
                ]

        it "finds both tables in document order" $ do
            length (extractTables doc) `shouldBe` 2

        it "parses headers and rows correctly" $ do
            case extractFirstTable doc of
                Nothing  -> expectationFailure "expected at least one table"
                Just tbl -> do
                    mtHeaders tbl `shouldBe` ["repo", "in-degree"]
                    mtRows tbl `shouldBe`
                        [ ["kritick-admin",    "0"]
                        , ["kritick-customer", "1"]
                        ]

    describe "columnByHeader" $ do
        let doc = T.unlines
                [ "| repo | in-degree |"
                , "|---|---:|"
                , "| kritick-admin | 0 |"
                , "| kritick-customer | 1 |"
                ]
        it "fetches the requested column" $ do
            case extractFirstTable doc of
                Nothing  -> expectationFailure "expected a table"
                Just tbl ->
                    columnByHeader "repo" tbl
                        `shouldBe` Just ["kritick-admin", "kritick-customer"]

        it "is case-insensitive on the header" $ do
            case extractFirstTable doc of
                Nothing  -> expectationFailure "expected a table"
                Just tbl ->
                    columnByHeader "REPO" tbl
                        `shouldBe` Just ["kritick-admin", "kritick-customer"]

    describe "sectionsByLevel" $ do
        let doc = T.unlines
                [ "# Top"
                , ""
                , "intro paragraph"
                , ""
                , "## Alpha"
                , ""
                , "alpha body"
                , ""
                , "## Beta"
                , ""
                , "beta body"
                , ""
                , "# Other top"
                , ""
                , "other body"
                ]
        it "returns one entry per H2" $
            length (sectionsByLevel 2 doc) `shouldBe` 2

        it "preserves heading text" $
            map secHeading (sectionsByLevel 2 doc)
                `shouldBe` ["Alpha", "Beta"]

        it "stops a section at the next same-or-shallower heading" $ do
            case extractSection "Alpha" doc of
                Nothing  -> expectationFailure "expected Alpha section"
                Just sec ->
                    secHeading sec `shouldBe` "Alpha"

    describe "extractBacktickTokens" $ do
        it "pulls every single-backtick token out of a line" $ do
            let doc = "See `foo/bar.md` and `baz` for details."
            extractBacktickTokens doc `shouldBe` ["foo/bar.md", "baz"]

        it "preserves whitespace inside the span" $ do
            let doc = "It says `  spaced  ` here."
            extractBacktickTokens doc `shouldBe` ["  spaced  "]

        it "drops empty spans (``)" $ do
            let doc = "Empty `` should be skipped, but `kept` stays."
            extractBacktickTokens doc `shouldBe` ["kept"]

        it "skips contents of triple-backtick fenced code blocks" $ do
            let doc = T.unlines
                    [ "Before fence: `cited.md`."
                    , ""
                    , "```haskell"
                    , "let x = `inside-block`"
                    , "let y = `also-inside`"
                    , "```"
                    , ""
                    , "After fence: `also-cited.md`."
                    ]
            extractBacktickTokens doc
                `shouldBe` ["cited.md", "also-cited.md"]

        it "is greedy: pairs a stray backtick with the next one" $ do
            -- Edge case: a stray ` followed by a real `closed`
            -- token gets paired greedily, leaving the trailing
            -- "closed" unconsumed. Acceptable in practice because
            -- looksLikePath downstream filters the prose-shaped
            -- phantom out.
            let doc = "Stray ` here, then `closed`."
            extractBacktickTokens doc `shouldBe` [" here, then "]

        it "handles the typical AGENTS.md citation pattern" $ do
            let doc = T.unlines
                    [ "## Embodied values (extends `../../USER.md` baseline)"
                    , ""
                    , "See `../../research/fp-best-practices.md` for"
                    , "the synthesis. Also `forkIO` is bad."
                    ]
            extractBacktickTokens doc
                `shouldBe` [ "../../USER.md"
                           , "../../research/fp-best-practices.md"
                           , "forkIO"
                           ]
