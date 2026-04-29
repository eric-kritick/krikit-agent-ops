{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | @krikit-regen-repo-inventory@: regenerate the categorized
-- agent-facing repo inventory at
-- @krikit-agent-fabric\/context\/repo-inventory.generated.md@.
--
-- Inputs:
--
-- 1. @--ecosystem-roots@ -- canonical kritick-* exhaustive list,
--    typically @kritick-ecosystem\/docs\/ecosystem-roots.generated.md@.
-- 2. @--workspace@ -- on-disk workspace root, walked one level
--    deep to catch every other repo prefix
--    (@krikit-*@, @purescript-*@, @scala-*@, @ng-*@, @react-*@,
--    @util-*@, etc.) plus surface anything uncategorized.
--
-- The two inputs union; duplicates are deduped and each category's
-- repo list sorted ascending. Idempotent write: substantive content
-- unchanged → file untouched.
module Main (main) where

import qualified Data.Text                                as T
import qualified Data.Text.IO                             as TIO
import qualified Data.Time                                as Time
import           Options.Applicative
    ( Parser
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , metavar
    , progDesc
    , short
    , strOption
    , value
    , (<**>)
    )
import           System.Exit                              (exitFailure)

import           System.IO                                (hPutStrLn, stderr)

import           Krikit.Agent.Ops.Regen.FsWalk            (listRepoDirs)
import           Krikit.Agent.Ops.Regen.Write
    ( WriteOutcome (..)
    , renderOutcome
    , writeIfChanged
    )
import           Krikit.Agent.Ops.RepoInventory.Run
    ( applyFilter
    , buildInventory
    , extractEcosystemRoots
    , readEcosystemFilter
    , renderInventory
    )

data Opts = Opts
    { optEcosystemRoots  :: !FilePath
    , optEcosystemConfig :: !FilePath
    , optWorkspaceRoot   :: !FilePath
    , optOutputMd        :: !FilePath
    }

optsParser :: Parser Opts
optsParser =
    Opts
        <$> strOption
            (  long  "ecosystem-roots"
            <> short 'e'
            <> metavar "PATH"
            <> value defaultRoots
            <> help "Path to ecosystem-roots.generated.md"
            )
        <*> strOption
            (  long  "ecosystem-config"
            <> short 'c'
            <> metavar "PATH"
            <> value defaultConfig
            <> help "Path to ecosystem.json (provides ignore + skip_repo_prefixes)"
            )
        <*> strOption
            (  long  "workspace"
            <> short 'w'
            <> metavar "DIR"
            <> value defaultWorkspace
            <> help "Workspace root to walk for non-kritick-* repos"
            )
        <*> strOption
            (  long  "output"
            <> short 'o'
            <> metavar "PATH"
            <> value defaultOutput
            <> help "Path to repo-inventory.generated.md"
            )
  where
    defaultRoots     = "/Users/opsadmin/Development/kritick-ecosystem/docs/ecosystem-roots.generated.md"
    defaultConfig    = "/Users/opsadmin/Development/kritick-ecosystem/config/ecosystem.json"
    defaultWorkspace = "/Users/opsadmin/Development"
    defaultOutput    = "/Users/opsadmin/Development/krikit-agent-fabric/context/repo-inventory.generated.md"

main :: IO ()
main = do
    Opts{..} <- execParser $
        info (optsParser <**> helper)
            (  fullDesc
            <> progDesc "Regenerate context/repo-inventory.generated.md"
            <> header   "krikit-regen-repo-inventory"
            )
    today    <- isoDateUTC

    rootsDoc <- TIO.readFile optEcosystemRoots
    let kritickRepos = extractEcosystemRoots rootsDoc

    fsRepos  <- listRepoDirs optWorkspaceRoot

    -- Honour kritick-ecosystem's existing ignore + skip_repo_prefixes
    -- (ecosystem_scan.py applies the same fields). If the config can't
    -- be read, fail loudly: an unfiltered inventory would silently
    -- include repos like fastlane-ios-certificates.
    filterE  <- readEcosystemFilter optEcosystemConfig
    cfgFilter <- case filterE of
        Right f  -> pure f
        Left err -> do
            hPutStrLn stderr ("FAIL: " <> T.unpack err)
            exitFailure

    let filtered = applyFilter cfgFilter (kritickRepos ++ fsRepos)
        inv      = buildInventory filtered
        body     = renderInventory today inv

    outcome <- writeIfChanged optOutputMd body
    TIO.putStrLn (renderOutcome optOutputMd outcome)
    case outcome of
        WriteError _ -> exitFailure
        _            -> pure ()

isoDateUTC :: IO T.Text
isoDateUTC = do
    now <- Time.getCurrentTime
    pure (T.pack (Time.showGregorian (Time.utctDay now)))
