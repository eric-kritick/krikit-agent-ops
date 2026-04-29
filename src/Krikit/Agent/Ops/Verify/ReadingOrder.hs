{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | @krikit-verify-reading-order@: walk every @AGENTS.md@ /
-- @IDENTITY.md@ under @krikit-agent-fabric\/agents\/@ and check
-- that every cited file actually exists.
--
-- Citation pattern in these documents is **backtick-wrapped
-- relative paths**, e.g.:
--
-- > See `../../research/fp-best-practices.md` for the synthesis.
-- > Extends `../../USER.md`.
--
-- not markdown @[link](url)@ form. The extractor pulls every
-- single-backtick token and filters to ones that look like file
-- paths (contain @/@, no @://@), then resolves each against the
-- citing file's directory and checks for existence on disk.
--
-- Every missing path is one 'Finding' at 'Error' severity --
-- broken cross-references in agent docs are real drift.
--
-- Heuristic limits: backticks containing only inline code (no
-- @/@) are skipped, so we don't false-positive on
-- @\`Map.lookup\`@ or @\`forkIO\`@. Bare top-level filenames like
-- @\`USER.md\`@ (no slash) are also skipped today; if those need
-- coverage, write them as @\`./USER.md\`@ or @\`../../USER.md\`@
-- in the source.
module Krikit.Agent.Ops.Verify.ReadingOrder
    ( -- * Pure helpers
      looksLikePath
    , stripAnchor
    , resolvePath
    , buildFindingsForFile

      -- * Effectful orchestrator
    , verify
    ) where

import           Data.List                                (isSuffixOf, nub)
import           Data.Text                                (Text)
import qualified Data.Text                                as T
import qualified Data.Text.IO                             as TIO
import           System.Directory                         (doesDirectoryExist,
                                                           doesPathExist)
import           System.FilePath                          (joinPath,
                                                           splitDirectories,
                                                           takeDirectory,
                                                           (</>))

import           Effectful                                (Eff, IOE, liftIO, (:>))
import           Effectful.Reader.Static                  (Reader, ask)

import           Krikit.Agent.Ops.Config
    ( Config (..)
    , FabricPaths (..)
    , PathsConfig (..)
    )
import           Krikit.Agent.Ops.Regen.FsWalk            (findFilesRec)
import           Krikit.Agent.Ops.Regen.MarkdownExtract   (extractBacktickTokens)
import           Krikit.Agent.Ops.Verify.Common           (Finding (..),
                                                           Severity (..))

-- =============================================================================
-- Pure helpers
-- =============================================================================

-- | Does this backtick token look like a file-path citation?
-- Heuristic:
--
--   * Contains a @/@.
--   * Does not contain @://@ (excludes URLs).
--   * No whitespace (excludes prose).
--   * Does not start with @-@ (excludes CLI flag tokens).
--   * Either ends in a recognized text-file extension, or ends
--     in @/@ (directory reference). This is the part that keeps
--     identifier-shaped tokens like @claude-cli/claude-opus-4-7@
--     and @openai-codex/gpt-5.5@ out of the result set despite
--     containing a slash.
--
-- The recognized-extension set is intentionally small (text files
-- the agent docs would actually cite). If a real citation uses
-- a less-common extension, extend 'pathLikeExtensions'.
looksLikePath :: Text -> Bool
looksLikePath t =
    let s = stripAnchor (T.strip t)
    in  not (T.null s)
     && "/" `T.isInfixOf` s
     && not ("://"  `T.isInfixOf` s)
     && not (T.any (\c -> c == ' ' || c == '\t' || c == '\n') s)
     && T.head s /= '-'
     && (T.last s == '/' || hasKnownExt s)

-- | Recognized text-file extensions the agent docs are likely to
-- cite. Conservative list; expand as new citation shapes appear.
pathLikeExtensions :: [Text]
pathLikeExtensions =
    [ ".md", ".json", ".hs", ".purs", ".scala", ".py"
    , ".sh", ".toml", ".yml", ".yaml", ".txt", ".js", ".ts"
    , ".cabal", ".plist", ".conf", ".csv", ".jsonl"
    ]

hasKnownExt :: Text -> Bool
hasKnownExt s = any (`T.isSuffixOf` s) pathLikeExtensions

-- | Strip a trailing @#anchor@, if any. Anchors are markdown
-- intra-document references; we only check the file half.
stripAnchor :: Text -> Text
stripAnchor t = T.takeWhile (/= '#') t

-- | Resolve a relative path against the citing file's directory.
-- Absolute paths (leading @/@) are returned with their @..@ /
-- @.@ segments collapsed; relative paths are joined to the base
-- and then collapsed.
--
-- We collapse manually rather than using @System.FilePath.normalise@
-- because @normalise@ leaves @..@ alone, which would let an
-- existence check happen against an unintuitive path.
resolvePath
    :: FilePath  -- ^ directory of the citing file
    -> Text      -- ^ relative path from the citation
    -> FilePath
resolvePath base raw =
    let s = T.unpack (stripAnchor raw)
    in  case s of
            ('/' : _) -> collapseDotDot s
            _         -> collapseDotDot (base </> s)

-- | Pure path canonicaliser: walk segments, drop @\".\"@,
-- collapse @\"..\"@ against the previous non-@..@ segment.
-- Below-root @..@ segments are preserved (no \"escape\" detection
-- here; @doesPathExist@ will simply return @False@).
collapseDotDot :: FilePath -> FilePath
collapseDotDot p =
    case splitDirectories p of
        []         -> ""
        ("/" : xs) -> "/" ++ joinPath (go [] xs)
        xs         -> joinPath (go [] xs)
  where
    go :: [FilePath] -> [FilePath] -> [FilePath]
    go acc []          = reverse acc
    go acc ("." : xs)  = go acc xs
    go acc (".." : xs) = case acc of
        (h : t) | h /= ".." -> go t xs
        _                   -> go (".." : acc) xs
    go acc (x : xs)    = go (x : acc) xs

-- | Pure core: given a citing file's path and its content, return
-- one 'Finding' per missing-citation. The caller threads the
-- existence check (we accept it as a function so the pure logic
-- stays testable without IO).
buildFindingsForFile
    :: FilePath                         -- ^ citing file's path
    -> Text                             -- ^ citing file's content
    -> [(FilePath, Bool)]               -- ^ resolved-path \/ exists?
    -> [Finding]
buildFindingsForFile citing _content existence =
    [ Finding
        { fSeverity = Error
        , fSubject  = T.pack citing
        , fMessage  = "cited path does not exist: " <> T.pack p
        }
    | (p, False) <- existence
    ]

-- =============================================================================
-- Effectful orchestrator
-- =============================================================================

-- | End-to-end verifier: walk @agents_dir@ for @AGENTS.md@ and
-- @IDENTITY.md@, parse each, resolve every backtick-citation,
-- check existence, and return findings.
verify
    :: ( Reader Config :> es
       , IOE :> es
       )
    => Eff es (Either Text [Finding])
verify = do
    cfg <- ask
    let agentsDir = fpAgentsDir . pcFabric . cfgPaths $ cfg
    exists <- liftIO (doesDirectoryExist agentsDir)
    if not exists
        then pure (Left ("agents_dir does not exist: " <> T.pack agentsDir))
        else do
            files    <- liftIO (findAgentsAndIdentityFiles agentsDir)
            allFinds <- liftIO (mapM (verifyOne agentsDir) files)
            pure (Right (concat allFinds))

-- | Walk @root@ recursively, collecting every file whose name is
-- @AGENTS.md@ or @IDENTITY.md@.
findAgentsAndIdentityFiles :: FilePath -> IO [FilePath]
findAgentsAndIdentityFiles root =
    findFilesRec root (\p ->
        "AGENTS.md"   `isSuffixOf` p
     || "IDENTITY.md" `isSuffixOf` p)

-- | Verify one citing file. Read its contents, extract backtick
-- tokens, filter to file-shaped, resolve each, check existence,
-- return findings.
verifyOne :: FilePath -> FilePath -> IO [Finding]
verifyOne _agentsRoot citing = do
    body <- TIO.readFile citing
    let baseDir   = takeDirectory citing
        candidates = filter looksLikePath (extractBacktickTokens body)
        -- One finding per unique missing path within this file --
        -- multiple citations of the same path don't multiply the
        -- noise.
        resolved   = nub (map (resolvePath baseDir) candidates)
    existence <- mapM checkOne resolved
    pure (buildFindingsForFile citing body existence)
  where
    checkOne :: FilePath -> IO (FilePath, Bool)
    checkOne p = do
        ok <- doesPathExist p
        pure (p, ok)
