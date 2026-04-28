{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | krikit-versions: report installed versions of every CLI in the
-- agent stack, grouped by user account (opsadmin, agentops, shared).
--
-- Pure-data driven: 'defaultSections' is a flat list of 'Subject'
-- entries, each describing one tool to inspect. Adding a new tool
-- means appending a row, not writing new logic.
module Krikit.Agent.Ops.Versions.Run
    ( -- * Domain types
      Section (..)
    , Subject (..)
    , VersionResult (..)

      -- * Default subject set
    , defaultSections

      -- * Runners
    , runAllSections
    , runSection
    , runSubject

      -- * Reporting
    , renderSection
    , renderAll
    ) where

import           Data.Text                      (Text)
import qualified Data.Text                      as T

import           Effectful                      (Eff, (:>))

import           Krikit.Agent.Ops.Effect.Proc
    ( Proc
    , ProcResult (..)
    , runCmd
    , runCmdAsUser
    )
import           Krikit.Agent.Ops.Units         (Seconds (..))

-- | A grouped block of tools in the report.
data Section = Section
    { secTitle    :: !Text
    , secSubjects :: ![Subject]
    }
    deriving stock (Eq, Show)

-- | One CLI to interrogate.
data Subject = Subject
    { subjLabel  :: !Text
    -- ^ Display name; what the operator sees in the report.
    , subjBinary :: !FilePath
    -- ^ Executable to invoke.
    , subjArgs   :: ![String]
    -- ^ Args passed to it (typically @["--version"]@; some tools differ).
    , subjAsUser :: !(Maybe Text)
    -- ^ 'Nothing' = run as the current user; @Just \"agentops\"@ runs
    -- via @sudo -u agentops -i ...@.
    }
    deriving stock (Eq, Show)

-- | What we got back from one inspection.
data VersionResult
    = VersionOk    !Text   -- ^ Version string (first line of output).
    | VersionMissing       -- ^ Subprocess failed; tool likely not installed.
    deriving stock (Eq, Show)

-- | Standard timeout per @--version@ probe. These are all near-instant
-- in the happy path; a long wait means something is hung.
versionTimeout :: Seconds
versionTimeout = Seconds 5

-- | Flat default list of every tool we want a version snapshot of.
defaultSections :: [Section]
defaultSections =
    [ Section "System (run as opsadmin)"
        [ Subject "macOS"    "sw_vers" ["-productVersion"] Nothing
        , Subject "brew"     "brew"    ["--version"]       Nothing
        ]
    , Section "opsadmin"
        [ Subject "node"     "node"    ["--version"]       Nothing
        , Subject "python3"  "python3" ["--version"]       Nothing
        , Subject "claude"   "claude"  ["--version"]       Nothing
        , Subject "codex"    "codex"   ["--version"]       Nothing
        , Subject "gh"       "gh"      ["--version"]       Nothing
        ]
    , Section "agentops"
        [ Subject "node"     "node"    ["--version"]       (Just "agentops")
        , Subject "openclaw" "openclaw" ["--version"]      (Just "agentops")
        , Subject "claude"   "claude"  ["--version"]       (Just "agentops")
        , Subject "codex"    "codex"   ["--version"]       (Just "agentops")
        ]
    , Section "Shared system"
        [ Subject "ollama"   "ollama"  ["--version"]       Nothing
        , Subject "colima"   "colima"  ["version"]         Nothing
        , Subject "docker"   "docker"  ["--version"]       Nothing
        ]
    ]

-- | Run every default section in order.
runAllSections :: (Proc :> es) => Eff es [(Section, [VersionResult])]
runAllSections = mapM runSection defaultSections

-- | Run every subject in a section, preserving order.
runSection
    :: (Proc :> es)
    => Section -> Eff es (Section, [VersionResult])
runSection sec = do
    rs <- mapM runSubject (secSubjects sec)
    pure (sec, rs)

-- | Run one subject. Pulls the first line of the command's stdout
-- (most CLIs print the version on line 1), trimmed.
runSubject :: (Proc :> es) => Subject -> Eff es VersionResult
runSubject s = do
    r <- case subjAsUser s of
        Nothing ->
            runCmd (subjBinary s) (subjArgs s) versionTimeout
        Just user ->
            runCmdAsUser user (subjBinary s) (subjArgs s) versionTimeout
    pure $ case r of
        Right ProcResult { prStdout = out } -> VersionOk (firstNonEmptyLine out)
        Left _                              -> VersionMissing

-- | Pick the first non-empty trimmed line from a chunk of text. Falls
-- back to empty on entirely-blank output.
firstNonEmptyLine :: Text -> Text
firstNonEmptyLine =
    case_ . map T.strip . T.lines
  where
    case_ = \case
        []     -> ""
        ("":r) -> case_ r
        (x:_)  -> x

-- === Rendering ===============================================================

-- | Format a single section as plain text.
renderSection :: Section -> [VersionResult] -> Text
renderSection sec rs =
    let header = "== " <> secTitle sec <> " =="
        rows   = zipWith line (secSubjects sec) rs
    in  T.intercalate "\n" (header : rows)
  where
    line subj r =
        "  "
            <> T.justifyLeft 12 ' ' (subjLabel subj)
            <> renderResult r

renderResult :: VersionResult -> Text
renderResult = \case
    VersionOk v     -> v
    VersionMissing  -> "(not installed)"

-- | Format every section, one after another, with blank-line separators.
renderAll :: [(Section, [VersionResult])] -> Text
renderAll =
    T.intercalate "\n\n" . map (uncurry renderSection)
