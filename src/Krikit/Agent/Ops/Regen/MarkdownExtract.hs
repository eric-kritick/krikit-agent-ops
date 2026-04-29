{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Markdown extraction primitives shared by every generator that
-- reads other @*.generated.md@ or hand-authored @.md@ files as inputs.
--
-- Two shapes show up over and over:
--
-- 1. Pipe tables -- each row is one record (@| col-a | col-b |@).
--    Used by @ecosystem-roots.generated.md@ to list repos with
--    their in-degree, by sync manifests, and by various human
--    runbooks. 'extractTables' returns every table in document
--    order; 'extractFirstTable' is the convenience pick.
--
-- 2. @## Heading@ sections -- prose under a heading. Used by RFCs
--    and design docs. 'extractSection' returns the body for one
--    heading; 'sectionsByLevel' returns a flat list keyed by
--    heading text.
--
-- The parser is intentionally line-based and forgiving: real
-- generated markdown drifts in whitespace, blank-line count, and
-- alignment column shape (@---@ vs @---:@ vs @:---:@). Treat this
-- as the verifier's tolerance level; if a generator produces text
-- this parser can't round-trip, the generator -- not the parser --
-- is wrong.
module Krikit.Agent.Ops.Regen.MarkdownExtract
    ( -- * Pipe tables
      MarkdownTable (..)
    , extractTables
    , extractFirstTable
    , columnByHeader

      -- * Sections
    , Section (..)
    , extractSection
    , sectionsByLevel

      -- * Helpers
    , trimCell
    , isAlignmentRow
    ) where

import           Data.Char           (isSpace)
import           Data.List           (find)
import           Data.Maybe          (mapMaybe)
import           Data.Text           (Text)
import qualified Data.Text           as T

-- =============================================================================
-- Pipe tables
-- =============================================================================

-- | One pipe table extracted from a markdown document.
data MarkdownTable = MarkdownTable
    { mtHeaders :: ![Text]   -- ^ trimmed header cells in order
    , mtRows    :: ![[Text]] -- ^ each row's trimmed cells in order
    }
    deriving stock (Eq, Show)

-- | Walk a markdown document and return every pipe table in order.
--
-- Recognition rule: a header row (line whose trimmed form starts
-- and ends with @|@) immediately followed by an alignment row
-- (cells made entirely of @-@, @:@, and whitespace). Body rows
-- continue until a non-table line. Blank lines end the table.
extractTables :: Text -> [MarkdownTable]
extractTables = goLines . T.lines
  where
    goLines :: [Text] -> [MarkdownTable]
    goLines [] = []
    goLines (l : rest)
        | isPipeRow l = case rest of
            (next : after)
                | isAlignmentRow next ->
                    let (rows, remaining) = collectBodyRows after
                        tbl = MarkdownTable
                                { mtHeaders = parsePipeRow l
                                , mtRows    = map parsePipeRow rows
                                }
                    in  tbl : goLines remaining
            _  -> goLines rest
        | otherwise = goLines rest

    collectBodyRows :: [Text] -> ([Text], [Text])
    collectBodyRows []       = ([], [])
    collectBodyRows (l : ls)
        | T.null (T.strip l)   = ([], ls)
        | isPipeRow l          =
            let (more, rest) = collectBodyRows ls
            in  (l : more, rest)
        | otherwise            = ([], l : ls)

-- | Convenience: first table in document order.
extractFirstTable :: Text -> Maybe MarkdownTable
extractFirstTable t = case extractTables t of
    []      -> Nothing
    (x : _) -> Just x

-- | Pull one column from a table by header name. Useful when callers
-- know they want @repo@ and don\'t care about column order.
columnByHeader :: Text -> MarkdownTable -> Maybe [Text]
columnByHeader header tbl = do
    idx <- findIndex (== T.toLower header) (map T.toLower (mtHeaders tbl))
    pure (mapMaybe (safeIndex idx) (mtRows tbl))
  where
    findIndex p = go 0
      where
        go _ []                 = Nothing
        go i (x : xs)
            | p x   = Just i
            | otherwise = go (i + 1) xs

    safeIndex i xs
        | i < length xs = Just (xs !! i)
        | otherwise     = Nothing

-- | A line that looks like a pipe-table row.
isPipeRow :: Text -> Bool
isPipeRow t =
    let s = T.strip t
    in  not (T.null s) && T.head s == '|' && T.last s == '|'

-- | A line that looks like a markdown alignment row, e.g.
-- @|---|---:|:---:|@. We accept any row whose every cell after
-- splitting is non-empty and made of @-@ / @:@ characters only.
isAlignmentRow :: Text -> Bool
isAlignmentRow t =
    let s = T.strip t
    in  isPipeRow s
            && all isAlignmentCell (parsePipeRow s)
  where
    isAlignmentCell c =
        let trimmed = T.strip c
        in  not (T.null trimmed)
              && T.all (`elem` ("-:" :: String)) trimmed

-- | Split a pipe row into its cells and trim each.
parsePipeRow :: Text -> [Text]
parsePipeRow t =
    map T.strip
        . dropEmptyEdges
        $ T.splitOn "|" (T.strip t)
  where
    dropEmptyEdges xs =
        let xs'  = case xs of (h : rest) | T.null h -> rest; _ -> xs
            xs'' = case reverse xs' of
                    (h : rest) | T.null h -> reverse rest
                    _                     -> xs'
        in  xs''

-- | Trim leading and trailing whitespace from a cell. Re-exported
-- because callers often want it after fishing values out by hand.
trimCell :: Text -> Text
trimCell = T.dropAround isSpace

-- =============================================================================
-- Sections
-- =============================================================================

-- | A markdown section: heading line plus body lines until the next
-- heading at the same or shallower level.
data Section = Section
    { secLevel   :: !Int     -- ^ number of leading @#@s
    , secHeading :: !Text    -- ^ heading text (without the @#@s)
    , secBody    :: !Text    -- ^ raw body, joined with newlines
    }
    deriving stock (Eq, Show)

-- | Return every section at the given heading level. Body of one
-- section ends at the next heading of the same or shallower level
-- (matching how a reader naturally chunks a document).
sectionsByLevel :: Int -> Text -> [Section]
sectionsByLevel level doc = go (T.lines doc)
  where
    go :: [Text] -> [Section]
    go [] = []
    go (l : rest)
        | Just (lvl, heading) <- parseHeading l, lvl == level =
            let (body, remaining) = takeBody rest
            in  Section { secLevel = lvl, secHeading = heading, secBody = T.intercalate "\n" body }
                : go remaining
        | otherwise = go rest

    takeBody :: [Text] -> ([Text], [Text])
    takeBody [] = ([], [])
    takeBody (l : ls)
        | Just (lvl, _) <- parseHeading l, lvl <= level =
            ([], l : ls)
        | otherwise =
            let (more, rest) = takeBody ls
            in  (l : more, rest)

-- | First section whose heading matches (case-insensitive, trimmed).
extractSection :: Text -> Text -> Maybe Section
extractSection heading doc =
    find matches (concatMap (`sectionsByLevel` doc) [1 .. 6])
  where
    matches s = T.toLower (T.strip (secHeading s)) == T.toLower (T.strip heading)

-- | Parse @## Foo bar@ into @(2, \"Foo bar\")@.
parseHeading :: Text -> Maybe (Int, Text)
parseHeading line =
    let stripped = T.stripStart line
        (hashes, rest) = T.span (== '#') stripped
        n = T.length hashes
    in  if n >= 1 && n <= 6 && not (T.null rest) && T.head rest == ' '
            then Just (n, T.strip (T.drop 1 rest))
            else Nothing
