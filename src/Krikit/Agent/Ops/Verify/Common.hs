{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Shared scaffolding for every @krikit-verify-*@ binary.
--
-- A verifier reads inputs (config, generated files, filesystem
-- state, etc.), looks for inconsistencies, and emits zero or more
-- 'Finding's. The binary's exit code is derived from the worst
-- severity in the set:
--
-- * 'Info' alone -> exit 0 (\"clean\").
-- * any 'Warning' -> exit 1 (\"clean enough to ignore today, but
--   surface\").
-- * any 'Error' -> exit 2 (\"action required\").
--
-- Verifiers should bias toward 'Error' for the things the operator
-- actually has to fix. Don't paper over real drift with 'Warning'.
module Krikit.Agent.Ops.Verify.Common
    ( -- * Domain types
      Severity (..)
    , Finding (..)

      -- * Set comparison
    , SetDiff (..)
    , compareSets

      -- * Rendering + exit semantics
    , maxSeverity
    , exitForFindings
    , renderFindings
    ) where

import           Data.List       (sort)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as T
import           System.Exit     (ExitCode (..))

-- =============================================================================
-- Domain types
-- =============================================================================

-- | How serious is one finding?
data Severity = Info | Warning | Error
    deriving stock (Eq, Ord, Show)

-- | A single discrepancy. Subject identifies *what* (a repo name,
-- a config key, a file path); message says *why*.
data Finding = Finding
    { fSeverity :: !Severity
    , fSubject  :: !Text
    , fMessage  :: !Text
    }
    deriving stock (Eq, Show)

-- =============================================================================
-- Set comparison
-- =============================================================================

-- | Result of comparing two sets, partitioning into the three
-- standard buckets.
data SetDiff a = SetDiff
    { sdLeftOnly  :: ![a]    -- ^ in @left@, not in @right@ (sorted)
    , sdRightOnly :: ![a]    -- ^ in @right@, not in @left@ (sorted)
    , sdBoth      :: ![a]    -- ^ in both (sorted)
    }
    deriving stock (Eq, Show)

-- | Compare two ordered sets and return a sorted partition.
compareSets :: Ord a => Set a -> Set a -> SetDiff a
compareSets l r =
    SetDiff
        { sdLeftOnly  = sort (Set.toList (l `Set.difference` r))
        , sdRightOnly = sort (Set.toList (r `Set.difference` l))
        , sdBoth      = sort (Set.toList (l `Set.intersection` r))
        }

-- =============================================================================
-- Severity / exit / rendering
-- =============================================================================

-- | Highest severity present in a finding list, or @'Info'@ if empty.
maxSeverity :: [Finding] -> Severity
maxSeverity []  = Info
maxSeverity fs  = maximum (map fSeverity fs)

-- | Exit code derived from the worst severity in the set.
--
-- > Info    -> ExitSuccess
-- > Warning -> ExitFailure 1
-- > Error   -> ExitFailure 2
--
-- Two distinct non-zero codes so the operator's wrapper can
-- differentiate \"investigate when convenient\" from \"act now\"
-- without parsing the report.
exitForFindings :: [Finding] -> ExitCode
exitForFindings fs = case maxSeverity fs of
    Info    -> ExitSuccess
    Warning -> ExitFailure 1
    Error   -> ExitFailure 2

-- | Plain-text report. One line per finding plus a summary
-- footer; designed for direct redirection to the verifier's log.
renderFindings :: Text -> [Finding] -> Text
renderFindings verifierName fs =
    T.unlines $
        ["# " <> verifierName, ""]
        ++ body
        ++ ["", footer]
  where
    body = case fs of
        [] -> ["  CLEAN: no findings."]
        _  -> map renderOne fs

    renderOne f =
        "  "
            <> sevTag (fSeverity f)
            <> " "
            <> fSubject f
            <> ": "
            <> fMessage f

    sevTag = \case
        Info    -> "[info] "
        Warning -> "[WARN] "
        Error   -> "[ERR ] "

    nFindings = length fs
    nWarn     = length (filter ((== Warning) . fSeverity) fs)
    nErr      = length (filter ((== Error)   . fSeverity) fs)
    footer    =
        "Summary: "
            <> T.pack (show nFindings) <> " findings ("
            <> T.pack (show nErr) <> " errors, "
            <> T.pack (show nWarn) <> " warnings)."
