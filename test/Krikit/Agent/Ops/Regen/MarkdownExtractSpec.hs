{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the markdown extraction primitives.
module Krikit.Agent.Ops.Regen.MarkdownExtractSpec (spec) where

import qualified Data.Text                                  as T
import           Test.Hspec

import           Krikit.Agent.Ops.Regen.MarkdownExtract
    ( MarkdownTable (..)
    , Section (..)
    , columnByHeader
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
