{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | @krikit-regen-repo-inventory@: regenerate the categorized
-- repo inventory.
--
-- Inputs:
--
--   * @kritick-ecosystem\/docs\/ecosystem-roots.generated.md@
--     -- canonical exhaustive list of @kritick-*@ root repos.
--   * Filesystem walk of the workspace root (default
--     @~opsadmin\/Development\/@) -- catches every other prefix
--     (@krikit-*@, @purescript-*@, @scala-*@, @ng-kritick-*@, etc.)
--     plus surfaces uncategorized repos for follow-up.
--
-- Output:
-- @krikit-agent-fabric\/context\/repo-inventory.generated.md@.
--
-- Categorization is rule-based: every repo name is matched against
-- 'classify' and emitted under the first rule that fires. New
-- categories require updating 'allCategories' + 'classify'; the
-- compiler's exhaustiveness check on 'Category' enforces consistency.
module Krikit.Agent.Ops.RepoInventory.Run
    ( -- * Domain types
      Category (..)
    , Inventory (..)
    , EcosystemFilter (..)

      -- * Pure logic
    , allCategories
    , categoryHeading
    , categoryBlurb
    , classify
    , repoNote
    , buildInventory
    , applyFilter
    , extractEcosystemRoots

      -- * IO
    , readEcosystemFilter

      -- * Rendering
    , renderInventory

      -- * Effectful orchestrator
    , regenerate
    ) where

import           Control.Exception                    (IOException, try)
import           Data.Aeson                           (FromJSON (..), (.:?))
import qualified Data.Aeson                           as A
import qualified Data.ByteString.Lazy                 as LBS
import           Data.List                            (nub, sort)
import qualified Data.Map.Strict                      as Map
import           Data.Map.Strict                      (Map)
import           Data.Maybe                           (fromMaybe)
import           Data.Set                             (Set)
import qualified Data.Set                             as Set
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as TIO
import qualified Data.Time                            as Time

import           Effectful                            (Eff, IOE, liftIO, (:>))
import           Effectful.Reader.Static              (Reader, ask)

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
import           Krikit.Agent.Ops.Regen.FsWalk        (listRepoDirs)
import           Krikit.Agent.Ops.Regen.Write
    ( WriteOutcome
    , writeIfChanged
    )
import           Krikit.Agent.Ops.Regen.MarkdownExtract
    ( MarkdownTable (..)
    , extractFirstTable
    )

-- =============================================================================
-- Categories
-- =============================================================================

-- | Categories the inventory groups repos into. Order here defines
-- the section order in the rendered output.
--
-- @CatUnknown@ is the catch-all and always renders last; if any
-- repo lands there, that's a signal the taxonomy needs an update
-- and the verifier should flag it.
data Category
    = CatKritickProduct           -- ^ @kritick-{admin,customer,manager,representative}@
    | CatKritickProductSensitive  -- ^ supporting product assets with sensitive contents
    | CatKritickService           -- ^ @kritick-{api,process,session,...}@ etc
    | CatKritickCoordination      -- ^ @kritick-ecosystem@, @kritick-feature@
    | CatKrikit                   -- ^ @krikit-*@
    | CatPureScriptKritick        -- ^ @purescript-kritick-*@
    | CatPureScriptFree           -- ^ @purescript-*-free@
    | CatPureScriptData           -- ^ @purescript-*-data@
    | CatPureScriptReact          -- ^ @purescript-react-*@
    | CatPureScriptOther          -- ^ all other @purescript-*@
    | CatScalaFree                -- ^ @scala-*-free@
    | CatScalaData                -- ^ @scala-*-data@ + @scala-kritick-*-schema@ etc.
    | CatScalaOther               -- ^ remaining @scala-*@
    | CatNgKritick                -- ^ @ng-kritick-*@ + the small ng-* known-name set
    | CatReact                    -- ^ @react-*@ / @react-kritick-*@
    | CatUtil                     -- ^ @util-*@
    | CatTemplate                 -- ^ @template-*@
    | CatBuildTooling             -- ^ build / deploy / lint / publish
    | CatSchemaRoots              -- ^ root-level schema repos (mongodb-kritick-schema etc.)
    | CatPromptAssets             -- ^ LLM prompt assets (e.g. aws-bedrock-prompts)
    | CatExperimental             -- ^ standalone / research projects, not yet wired in
    | CatUnknown                  -- ^ catch-all (should be empty in steady state)
    deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Every category in render order.
allCategories :: [Category]
allCategories = [minBound .. maxBound]

-- | Markdown @## ...@ heading for a category.
categoryHeading :: Category -> Text
categoryHeading = \case
    CatKritickProduct          -> "kritick-* — product SPAs"
    CatKritickProductSensitive -> "kritick-* — supporting assets (sensitive)"
    CatKritickService          -> "kritick-* — services"
    CatKritickCoordination     -> "kritick-* — coordination"
    CatKrikit                  -> "krikit-* — ops / agent infrastructure"
    CatPureScriptKritick       -> "purescript-kritick-* — kritick-domain libraries"
    CatPureScriptFree          -> "purescript-*-free — Free-monad interpreters"
    CatPureScriptData          -> "purescript-*-data — pure data / value-type libraries"
    CatPureScriptReact         -> "purescript-react-* — React bindings"
    CatPureScriptOther         -> "purescript-* — other libraries"
    CatScalaFree               -> "scala-*-free — Free-monad interpreters (server-side)"
    CatScalaData               -> "scala-*-data / scala-kritick-* — domain types and schemas"
    CatScalaOther              -> "scala-* — other Scala libraries"
    CatNgKritick               -> "ng-kritick-* — Angular wrappers (legacy + active)"
    CatReact                   -> "react-* / react-kritick-* — React wrappers"
    CatUtil                    -> "util-* — utility tooling"
    CatTemplate                -> "template-* — repo templates"
    CatBuildTooling            -> "Build / deploy tooling"
    CatSchemaRoots             -> "Schema repos (root-level)"
    CatPromptAssets            -> "Prompt / model asset repos"
    CatExperimental            -> "Experimental / pre-integration"
    CatUnknown                 -> "Uncategorized"

-- | One-paragraph description rendered under the heading. Empty
-- string means \"no blurb\"; the renderer omits the line.
categoryBlurb :: Category -> Text
categoryBlurb = \case
    CatKritickProduct          -> "Customer-facing SPAs: admin, customer, manager, representative."
    CatKritickProductSensitive -> "Supporting assets for kritick product repos with sensitive contents (signing certificates, private keys). **Inventoried by name only -- do NOT access contents.**"
    CatKritickService          -> "Backend and supporting services in the kritick product fleet."
    CatKritickCoordination     -> "Cross-repo coordination and skills."
    CatKrikit                  -> "Top-level ops and agent infrastructure repos."
    CatPureScriptKritick       -> "Data types, schemas, React components, programs that encapsulate kritick domain logic."
    CatPureScriptFree          -> "Each hides one volatile external decision behind a sum-type ADT, interpreting to Aff at the edges."
    CatPureScriptData          -> "Domain types not tied to a specific external integration."
    CatPureScriptReact         -> "PureScript wrappers for React libraries and components."
    CatPureScriptOther         -> "Non-domain, non-React PureScript libraries."
    CatScalaFree               -> "Server-side parallels to purescript-*-free."
    CatScalaData               -> ""
    CatScalaOther              -> ""
    CatNgKritick               -> "Pattern: ng-kritick-<feature>. Mostly legacy; some still active."
    CatReact                   -> ""
    CatUtil                    -> ""
    CatTemplate                -> ""
    CatBuildTooling            -> ""
    CatSchemaRoots             -> "Schema definitions outside the kritick-*-schema pattern."
    CatPromptAssets            -> "Curated prompt corpora and similar non-code asset repos consumed by LLM tooling."
    CatExperimental            -> "Standalone or research projects under evaluation, not yet wired into the kritick / krikit ecosystem proper. Listed for awareness; expect status to change."
    CatUnknown                 -> "Repos that do not match any known prefix rule. If anything lands here, either add it to `kritick-ecosystem/config/ecosystem.json` `ignore` (if it should be excluded) or extend the taxonomy in `Krikit.Agent.Ops.RepoInventory.Run.classify`."

-- | Optional inline note rendered next to a specific repo entry.
-- Used for caveats that are repo-specific rather than category-wide
-- (e.g. \"sensitive: do not access contents\").
repoNote :: Text -> Maybe Text
repoNote = \case
    "fastlane-ios-certificates" ->
        Just "iOS signing certificates for `kritick-customer`. **Sensitive — do not access contents.**"
    _ -> Nothing

-- =============================================================================
-- Classification
-- =============================================================================

-- | Map a repo name to its category. Order of rules matters --
-- more-specific patterns must be matched before more-general ones.
classify :: Text -> Category
classify name
    -- Specific kritick repos (coordination is a tiny known set)
    | name `elem` kritickCoordRepos       = CatKritickCoordination
    | name `elem` kritickProductRepos     = CatKritickProduct
    | name `elem` kritickSensitiveRepos   = CatKritickProductSensitive

    -- Other ecosystem-relevant known names that don't fit a prefix rule
    | name `elem` buildToolingRepos       = CatBuildTooling
    | name `elem` schemaRootRepos         = CatSchemaRoots
    | name `elem` promptAssetRepos        = CatPromptAssets
    | name `elem` experimentalRepos       = CatExperimental
    | name `elem` ngKritickAdjacentRepos  = CatNgKritick

    -- Remaining kritick-* are services
    | "kritick-" `T.isPrefixOf` name      = CatKritickService

    -- krikit-* (note the spelling difference: krikit, not kritick)
    | "krikit-" `T.isPrefixOf` name       = CatKrikit

    -- PureScript: purescript-kritick-* before generic purescript-*-data etc.
    | "purescript-kritick-" `T.isPrefixOf` name = CatPureScriptKritick
    | "purescript-react-"   `T.isPrefixOf` name = CatPureScriptReact
    | "purescript-" `T.isPrefixOf` name && "-free" `T.isSuffixOf` name = CatPureScriptFree
    | "purescript-" `T.isPrefixOf` name && "-data" `T.isSuffixOf` name = CatPureScriptData
    | "purescript-" `T.isPrefixOf` name   = CatPureScriptOther

    -- Scala
    | "scala-" `T.isPrefixOf` name && "-free" `T.isSuffixOf` name = CatScalaFree
    | "scala-" `T.isPrefixOf` name && "-data" `T.isSuffixOf` name = CatScalaData
    | "scala-kritick-" `T.isPrefixOf` name = CatScalaData
    | "scala-" `T.isPrefixOf` name        = CatScalaOther

    -- Angular
    | "ng-kritick-" `T.isPrefixOf` name   = CatNgKritick

    -- React
    | "react-" `T.isPrefixOf` name        = CatReact

    -- Utility
    | "util-" `T.isPrefixOf` name         = CatUtil

    -- Templates
    | "template-" `T.isPrefixOf` name     = CatTemplate

    -- Catch-all
    | otherwise                           = CatUnknown

kritickCoordRepos :: [Text]
kritickCoordRepos =
    [ "kritick-ecosystem"
    , "kritick-feature"
    ]

kritickProductRepos :: [Text]
kritickProductRepos =
    [ "kritick-admin"
    , "kritick-customer"
    , "kritick-manager"
    , "kritick-representative"
    ]

-- | Supporting assets for product repos with sensitive contents.
-- Listed by name; the inventory acknowledges they exist but the
-- 'repoNote' machinery flags them \"do not access\".
kritickSensitiveRepos :: [Text]
kritickSensitiveRepos =
    [ "fastlane-ios-certificates"
    ]

buildToolingRepos :: [Text]
buildToolingRepos =
    [ "aws-cloudformation-kritick"
    , "aws-deploy-kritick"
    , "aws-deploy-kritick-webpack-plugin"
    , "aws-publish-spa-kritick-webpack-plugin"
    , "eslint-config-kritick"
    , "git-bower"
    , "git-hooks"
    , "npm-build-version-kritick"
    , "rollup-plugin-aws-deploy-kritick"
    , "static-assets-kritick"
    , "static-legal-kritick"
    ]

schemaRootRepos :: [Text]
schemaRootRepos =
    [ "elasticsearch-kritick-schema"
    , "mongodb-kritick-schema"
    , "redis-kritick-schema"
    ]

-- | Prompt / model asset repos (not source code; kept on disk for
-- LLM tooling).
promptAssetRepos :: [Text]
promptAssetRepos =
    [ "aws-bedrock-prompts"
    ]

-- | Standalone or research projects sitting in the workspace but
-- not yet integrated into the kritick / krikit ecosystem. Status
-- can change; revisit on each scan.
experimentalRepos :: [Text]
experimentalRepos =
    [ "autocoder"
    ]

-- | Active ng-* repos that do not match the @ng-kritick-@ prefix
-- but are part of the ecosystem. Folded into 'CatNgKritick'.
ngKritickAdjacentRepos :: [Text]
ngKritickAdjacentRepos =
    [ "ng-safe-apply"
    ]

-- =============================================================================
-- EcosystemFilter (consults kritick-ecosystem/config/ecosystem.json)
-- =============================================================================

-- | Filter rules read from @kritick-ecosystem\/config\/ecosystem.json@.
-- Two fields the generator respects:
--
--   * @ignore@ -- exact repo names to drop entirely.
--   * @skip_repo_prefixes@ -- prefixes to drop entirely.
--
-- These match the semantics of the existing @ecosystem_scan.py@
-- (lines 693, 710-711): a repo in either set is stripped from the
-- scan before any roots / graph / inventory processing.
data EcosystemFilter = EcosystemFilter
    { efIgnoreNames    :: !(Set Text)
    , efIgnorePrefixes :: ![Text]
    }
    deriving stock (Eq, Show)

instance FromJSON EcosystemFilter where
    parseJSON = A.withObject "EcosystemConfig" $ \o -> do
        ignores  <- fromMaybe [] <$> o .:? "ignore"
        prefixes <- fromMaybe [] <$> o .:? "skip_repo_prefixes"
        pure EcosystemFilter
            { efIgnoreNames    = Set.fromList ignores
            , efIgnorePrefixes = prefixes
            }

-- | Read filter rules from @ecosystem.json@. On any I/O or parse
-- error returns 'Left' with a human message; the caller decides
-- whether to fall back to an empty filter or fail outright.
readEcosystemFilter :: FilePath -> IO (Either Text EcosystemFilter)
readEcosystemFilter path = do
    result <- try (LBS.readFile path)
    case result of
        Left (e :: IOException) ->
            pure (Left ("could not read " <> T.pack path <> ": " <> T.pack (show e)))
        Right bytes -> case A.eitherDecode bytes of
            Left err  -> pure (Left ("could not parse " <> T.pack path <> ": " <> T.pack err))
            Right cfg -> pure (Right cfg)

-- | Drop ignored repos from the input list before classification.
applyFilter :: EcosystemFilter -> [Text] -> [Text]
applyFilter EcosystemFilter{..} = filter keep
  where
    keep n = not (n `Set.member` efIgnoreNames)
          && not (any (`T.isPrefixOf` n) efIgnorePrefixes)

-- =============================================================================
-- Inventory
-- =============================================================================

-- | The classified inventory: repos grouped under each category, in
-- the canonical 'allCategories' order, with each category\'s repo
-- list sorted ascending.
newtype Inventory = Inventory
    { invByCategory :: Map Category [Text]
    }
    deriving stock (Eq, Show)

-- | Build the inventory from a flat repo-name list. Duplicates are
-- collapsed, results sorted.
buildInventory :: [Text] -> Inventory
buildInventory names =
    Inventory
        { invByCategory =
            Map.map (sort . nub) $
                foldl' insertOne Map.empty names
        }
  where
    insertOne acc n =
        Map.insertWith (++) (classify n) [n] acc

-- | Pull the @repo@ column out of @ecosystem-roots.generated.md@.
-- The file's table layout is fixed by the generator on the
-- ecosystem side; if it changes we want a *parse* failure, not a
-- silent empty list, so we look up by header name.
extractEcosystemRoots :: Text -> [Text]
extractEcosystemRoots doc = case extractFirstTable doc of
    Nothing  -> []
    Just tbl -> case headerIndex "repo" (mtHeaders tbl) of
        Nothing  -> []
        Just idx ->
            filter (not . T.null)
                . map (cleanRepoCell . safeIdx idx)
                $ mtRows tbl
  where
    headerIndex :: Text -> [Text] -> Maybe Int
    headerIndex h hs = go 0 (map T.toLower hs)
      where
        go _ [] = Nothing
        go i (x : xs)
            | x == T.toLower h = Just i
            | otherwise        = go (i + 1) xs

    safeIdx :: Int -> [Text] -> Text
    safeIdx i row
        | i < length row = row !! i
        | otherwise      = ""

    -- A repo cell may be a markdown link: @[name](url)@. Strip if so.
    cleanRepoCell :: Text -> Text
    cleanRepoCell t =
        let stripped = T.strip t
        in  case T.stripPrefix "[" stripped of
                Just rest -> T.takeWhile (/= ']') rest
                Nothing   -> stripped

-- =============================================================================
-- Rendering
-- =============================================================================

renderInventory :: Text -> Inventory -> Text
renderInventory isoDate inv =
    T.unlines $
        [ "# Repo inventory (categorized)"
        , ""
        , renderBanner banner
        , ""
        , introBlurb
        , ""
        ]
        ++ concatMap renderCategory allCategories
  where
    banner = Banner
        { banGeneratorName = "krikit-regen-repo-inventory"
        , banInputs        =
            [ BannerInputs "`kritick-ecosystem/docs/ecosystem-roots.generated.md` (canonical kritick-* roots)"
            , BannerInputs "filesystem walk of the configured workspace root (default `~opsadmin/Development/`)"
            ]
        , banTrigger       = "on `ecosystem-roots.generated.md` change + cron 06:35 daily fallback"
        , banLastGenerated = isoDate
        }

    introBlurb = T.unlines
        [ "Categorized view of the kritick / krikit repo ecosystem."
        , "The canonical exhaustive inventory with metadata lives in"
        , "`kritick-ecosystem/docs/ecosystem-roots.generated.md`;"
        , "this file is the agent-facing categorized derived view."
        , ""
        , "For category definitions and naming conventions, see"
        , "`kritick-domain-knowledge.md`."
        ]

    renderCategory cat =
        case Map.lookup cat (invByCategory inv) of
            Nothing    -> []          -- empty bucket: omit the section entirely
            Just repos ->
                [ "## " <> categoryHeading cat
                , ""
                ]
                ++ blurbLines cat
                ++ map renderEntry repos
                ++ [""]

    renderEntry r = case repoNote r of
        Nothing   -> "- `" <> r <> "`"
        Just note -> "- `" <> r <> "` — " <> note

    blurbLines cat =
        let b = categoryBlurb cat
        in  if T.null b then [] else [b, ""]

-- =============================================================================
-- Effectful orchestrator
-- =============================================================================

-- | End-to-end regen: pull paths from @'Reader' 'Config'@, read the
-- canonical inputs, classify, write idempotently. Returns
-- @'Left' err@ on input failure or @'Right' outcome@ on success.
regenerate
    :: ( Reader Config :> es
       , IOE :> es
       )
    => Eff es (Either Text (FilePath, WriteOutcome))
regenerate = do
    cfg <- ask
    let rootsPath = epEcosystemRootsMd  . pcEcosystem . cfgPaths $ cfg
        ecoPath   = epEcosystemJson     . pcEcosystem . cfgPaths $ cfg
        outPath   = fpRepoInventoryMd   . pcFabric    . cfgPaths $ cfg
        wsRoot    = cfgWorkspaceRoot cfg

    today    <- liftIO isoDateUTC
    rootsDoc <- liftIO (TIO.readFile rootsPath)
    fsRepos  <- liftIO (listRepoDirs wsRoot)
    filterE  <- liftIO (readEcosystemFilter ecoPath)
    case filterE of
        Left err -> pure (Left err)
        Right f  -> do
            let kritickRepos = extractEcosystemRoots rootsDoc
                allRepos     = applyFilter f (kritickRepos ++ fsRepos)
                inv          = buildInventory allRepos
                body         = renderInventory today inv
            outcome <- liftIO (writeIfChanged outPath body)
            pure (Right (outPath, outcome))

-- | Today\'s date in @YYYY-MM-DD@. Used only for the banner\'s
-- @Last generated@ line; the writer scrubs that line during
-- idempotency comparison.
isoDateUTC :: IO Text
isoDateUTC = do
    now <- Time.getCurrentTime
    pure (T.pack (Time.showGregorian (Time.utctDay now)))
