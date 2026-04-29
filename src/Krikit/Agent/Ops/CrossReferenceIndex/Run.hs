{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | @krikit-regen-cross-reference-index@: emit the
-- agent-facing graph of \"which markdown file cites which\".
--
-- Inputs:
--
--   * @paths.fabric.root@ -- recursively walked for every @*.md@
--     citing file.
--   * @paths.ecosystem.docs_dir@ -- recursively walked for every
--     @*.md@ citing file.
--
-- Output:
-- @paths.fabric.cross_reference_index_md@. A single sorted
-- markdown table of @(source, target)@ pairs. Sorted by source,
-- then by target, so:
--
--   * Forward lookup (\"what does @X@ reference?\"):
--     @rg \"^\\| .X.\" cross-reference-index.generated.md@
--   * Reverse lookup (\"what cites @Y@?\"):
--     @rg \"\\| .Y. \\|$\" cross-reference-index.generated.md@
--
-- The cited target is the literal backtick-wrapped string from
-- the source -- we don't resolve relative paths here. The
-- reading-order verifier handles broken-link detection
-- separately; this generator's job is just to mirror the graph.
--
-- Source paths are emitted relative to @workspace_root@ so the
-- index is portable across machines (mini, dev box, future
-- nodes). Anchor suffixes like @\#section@ are stripped from
-- targets but the rest of the citation is left as the author
-- wrote it.
module Krikit.Agent.Ops.CrossReferenceIndex.Run
    ( -- * Domain types
      CrossRef (..)

      -- * Pure logic
    , extractRefsFromContent
    , renderIndex

      -- * Effectful orchestrator
    , regenerate
    ) where

import           Data.List                                (isSuffixOf, nub, sort)
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import qualified Data.Text.IO                             as TIO
import qualified Data.Time                                as Time
import           System.FilePath                          (makeRelative)

import           Effectful                                (Eff, IOE, liftIO, (:>))
import           Effectful.Reader.Static                  (Reader, ask)

import           Krikit.Agent.Ops.Config
    ( Config (..)
    , EcosystemPaths (..)
    , FabricPaths (..)
    , PathsConfig (..)
    )
import           Krikit.Agent.Ops.Regen.Banner
    ( Banner (..)
    , BannerInputs (..)
    , renderBanner
    )
import           Krikit.Agent.Ops.Regen.FsWalk            (findFilesRec)
import           Krikit.Agent.Ops.Regen.MarkdownExtract   (extractBacktickTokens)
import           Krikit.Agent.Ops.Regen.Write
    ( WriteOutcome
    , writeIfChanged
    )
import           Krikit.Agent.Ops.Verify.ReadingOrder     (looksLikePath,
                                                           stripAnchor)

-- =============================================================================
-- Domain types
-- =============================================================================

-- | One edge in the citation graph.
data CrossRef = CrossRef
    { xrSource :: !Text
      -- ^ workspace-relative path of the citing file.
    , xrTarget :: !Text
      -- ^ literal cited path (anchor stripped). Not resolved.
    }
    deriving stock (Eq, Ord, Show)

-- =============================================================================
-- Pure logic
-- =============================================================================

-- | Given a (workspace-relative) source path and the file's raw
-- content, return one 'CrossRef' per file-shaped backtick
-- citation. Targets are deduped within the file.
extractRefsFromContent
    :: Text     -- ^ workspace-relative source path
    -> Text     -- ^ source file content
    -> [CrossRef]
extractRefsFromContent srcPath content =
    [ CrossRef { xrSource = srcPath, xrTarget = stripAnchor t }
    | t <- nub (filter looksLikePath (extractBacktickTokens content))
    ]

-- | Render the markdown table of cross-references.
renderIndex
    :: Text       -- ^ ISO date for the @Last generated@ banner line
    -> [CrossRef]
    -> Text
renderIndex isoDate refs =
    T.unlines $
        [ "# Cross-reference index"
        , ""
        , renderBanner banner
        , ""
        , introBlurb
        , ""
        , tableHeader
        ]
        ++ map renderRow (sort refs)
  where
    banner = Banner
        { banGeneratorName = "krikit-regen-cross-reference-index"
        , banInputs        =
            [ BannerInputs "recursive walk of `krikit-agent-fabric/` for `*.md`"
            , BannerInputs "recursive walk of `kritick-ecosystem/docs/` for `*.md`"
            ]
        , banTrigger       = "cron 06:40 daily"
        , banLastGenerated = isoDate
        }

    introBlurb = T.unlines
        [ "Edges in the citation graph: every backtick-wrapped"
        , "file-shaped path found in any `*.md` under the two"
        , "input roots. Sorted by source, then by target."
        , ""
        , "Lookups:"
        , ""
        , "- Forward (\"what does X reference?\"):"
        , "  `rg \"^\\| .X. \" cross-reference-index.generated.md`"
        , "- Reverse (\"what cites Y?\"):"
        , "  `rg \"\\| .Y. \\|$\" cross-reference-index.generated.md`"
        , ""
        , "The target column carries the citation as the author"
        , "wrote it (with anchors stripped). For broken-link"
        , "detection, see `krikit-verify-reading-order`."
        ]

    tableHeader = T.unlines
        [ "| Source | Target |"
        , "|---|---|"
        ]

    renderRow r = "| `" <> xrSource r <> "` | `" <> xrTarget r <> "` |"

-- =============================================================================
-- Effectful orchestrator
-- =============================================================================

-- | End-to-end regen. Walk both roots, parse each markdown,
-- collect refs, sort, render, write idempotently.
regenerate
    :: ( Reader Config :> es
       , IOE :> es
       )
    => Eff es (Either Text (FilePath, WriteOutcome))
regenerate = do
    cfg <- ask
    let fabricRoot = fpRoot                  . pcFabric    . cfgPaths $ cfg
        ecoDocs    = epDocsDir               . pcEcosystem . cfgPaths $ cfg
        outPath    = fpCrossReferenceIndexMd . pcFabric    . cfgPaths $ cfg
        wsRoot     = cfgWorkspaceRoot cfg

    today    <- liftIO isoDateUTC
    fabricMd <- liftIO (findFilesRec fabricRoot isMarkdown)
    ecoMd    <- liftIO (findFilesRec ecoDocs    isMarkdown)
    let allMd = fabricMd ++ ecoMd

    refs     <- liftIO (concat <$> mapM (refsForFile wsRoot) allMd)
    let body = renderIndex today refs
    outcome <- liftIO (writeIfChanged outPath body)
    pure (Right (outPath, outcome))
  where
    isMarkdown :: FilePath -> Bool
    isMarkdown p = ".md" `isSuffixOf` p

-- | Read one file's content, parse refs, render the source path
-- as workspace-relative.
refsForFile :: FilePath -> FilePath -> IO [CrossRef]
refsForFile wsRoot path = do
    body <- TIO.readFile path
    let rel = T.pack (makeRelative wsRoot path)
    pure (extractRefsFromContent rel body)

-- | Today's date in @YYYY-MM-DD@. Used only for the banner;
-- normalized away by the writer's idempotency comparison.
isoDateUTC :: IO Text
isoDateUTC = do
    now <- Time.getCurrentTime
    pure (T.pack (Time.showGregorian (Time.utctDay now)))
