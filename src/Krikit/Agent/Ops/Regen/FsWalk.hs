{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Filesystem walk helpers for generators that classify on-disk
-- repos. Intentionally minimal: shallow listing of one directory,
-- filtering to entries that \"look like\" repo dirs (subdir with a
-- @.git@ child entry, file or directory). No recursion, no globbing
-- -- a workspace root is a flat list of repo siblings under our
-- convention.
module Krikit.Agent.Ops.Regen.FsWalk
    ( -- * Operations
      listRepoDirs
    , listSubdirs
    , findFilesRec
    ) where

import           Control.Exception        (IOException, try)
import           Control.Monad            (filterM)
import           Data.List                (sort)
import qualified Data.Text                as T
import           Data.Text                (Text)
import           System.Directory
    ( doesDirectoryExist
    , doesPathExist
    , listDirectory
    )
import           System.FilePath          ((</>))

-- | Names of immediate subdirectories under @root@ that look like
-- git repos (they contain a @.git@ entry -- directory for normal
-- repos, file for submodules and worktrees). Returned as 'Text' for
-- ergonomics in markdown rendering.
--
-- Hidden entries (starting with @.@) are skipped.
listRepoDirs :: FilePath -> IO [Text]
listRepoDirs root = do
    subs <- listSubdirs root
    repos <- filterM (looksLikeRepo root) subs
    pure (sort (map T.pack repos))

-- | Plain immediate subdirectories of @root@, hidden entries dropped.
-- Returns 'IOException's as an empty list -- callers needing the
-- error detail should use 'try' themselves; in practice an
-- unreadable workspace root is itself a config bug we want surfaced
-- upstream.
listSubdirs :: FilePath -> IO [String]
listSubdirs root = do
    result <- try (listDirectory root)
    case result of
        Left (_ :: IOException) -> pure []
        Right entries           -> do
            let visible = filter (not . isHidden) entries
            filterM (\e -> doesDirectoryExist (root </> e)) visible
  where
    isHidden ('.' : _) = True
    isHidden _         = False

-- | A subdirectory @name@ under @root@ is \"a repo\" if a @.git@
-- entry of any kind exists at @root\/name\/.git@.
looksLikeRepo :: FilePath -> String -> IO Bool
looksLikeRepo root name = doesPathExist (root </> name </> ".git")

-- | Recursively find files under @root@ whose path satisfies the
-- predicate. Sorted output for stable downstream behavior.
-- Hidden entries (leading @.@) are skipped at every level.
--
-- Used by both the cross-reference-index generator (find every
-- @*.md@ under fabric) and the reading-order verifier (find every
-- @AGENTS.md@ \/ @IDENTITY.md@ under fabric\/agents). Pure Haskell;
-- no shelling out to @find@ \/ @rg --files@.
findFilesRec :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
findFilesRec root pred_ = do
    entries <- listDirectorySafe root
    let visible = filter (not . isHidden) entries
    found    <- mapM (dispatch . (root </>)) visible
    pure (sort (concat found))
  where
    dispatch :: FilePath -> IO [FilePath]
    dispatch p = do
        isDir <- doesDirectoryExist p
        if isDir
            then findFilesRec p pred_
            else pure ([p | pred_ p])

    isHidden :: FilePath -> Bool
    isHidden ('.' : _) = True
    isHidden _         = False

    listDirectorySafe :: FilePath -> IO [FilePath]
    listDirectorySafe p = do
        r <- try (listDirectory p)
        case r of
            Left (_ :: IOException) -> pure []
            Right xs                -> pure xs
