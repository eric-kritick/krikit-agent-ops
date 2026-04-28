{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Aggregate agent + model usage from the OpenClaw detailed log.
--
-- Replaces the @krikit-summary@ bash script. Strategy is identical:
-- tail the latest @\/tmp\/openclaw\/openclaw-YYYY-MM-DD.log@ and pull
-- out @agent:NAME@ and @\<provider\>\/\<model\>@ tokens, count them,
-- sort by count, plus a separate count for escalations to thinker
-- or builder.
module Krikit.Agent.Ops.Summary.Run
    ( -- * Domain types
      SummaryReport (..)
    , SummaryConfig (..)
    , defaultSummaryConfig

      -- * Pure helpers (testable)
    , extractAgentRefs
    , extractModelRefs
    , countOccurrences
    , knownProviders
    , countEscalations

      -- * Orchestrator (uses Proc)
    , runSummary
    , latestLogPath

      -- * Reporting
    , renderReport
    ) where

import           Data.Char                      (isAsciiLower, isAsciiUpper, isDigit)
import           Data.List                      (sortBy)
import qualified Data.Map.Strict                as Map
import           Data.Ord                       (Down (..), comparing)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Time                      as Time

import           Effectful                      (Eff, IOE, liftIO, (:>))

import           Krikit.Agent.Ops.Effect.Proc
    ( Proc
    , ProcResult (..)
    , runCmd
    )
import           Krikit.Agent.Ops.Units         (Seconds (..))

-- | One-shot config for a summary run. Currently just the line count
-- to tail; future fields could include log directory, provider list,
-- etc.
data SummaryConfig = SummaryConfig
    { scLineCount :: !Int
    }
    deriving stock (Eq, Show)

defaultSummaryConfig :: SummaryConfig
defaultSummaryConfig = SummaryConfig { scLineCount = 5000 }

-- | Aggregated counts produced by 'runSummary'. Lists are sorted in
-- descending order of count so the formatter just iterates.
data SummaryReport = SummaryReport
    { srAgents       :: ![(Text, Int)]
    , srModels       :: ![(Text, Int)]
    , srEscalations  :: !Int
    -- ^ Count of @agent:thinker@ + @agent:builder@ matches.
    , srLogPath      :: !Text
    , srLineCount    :: !Int
    }
    deriving stock (Eq, Show)

-- | Provider names we care about as the lefthand side of
-- @\<provider\>\/\<model\>@ matches. Mirrors the bash extended-regex.
knownProviders :: [Text]
knownProviders =
    [ "ollama"
    , "claude-cli"
    , "openai-codex"
    , "anthropic"
    , "deepseek"
    ]

-- | Find every @agent:NAME@ token in the input. NAME runs as long as
-- characters keep matching the agent-name shape (ASCII letters).
-- Returns the matches with their @agent:@ prefix preserved so the
-- formatter has stable, grep-compatible labels.
extractAgentRefs :: Text -> [Text]
extractAgentRefs = findAllWithPrefix "agent:" isAgentChar
  where
    isAgentChar c = isAsciiLower c || isAsciiUpper c

-- | Find every @\<provider\>\/\<model\>@ token for any of our
-- 'knownProviders'. Model names are ASCII alphanumerics with @:@,
-- @.@, @_@, and @-@ allowed (matching the bash regex).
extractModelRefs :: Text -> [Text]
extractModelRefs txt =
    concatMap (\p -> findAllWithPrefix (p <> "/") isModelChar txt) knownProviders
  where
    isModelChar c =
        isAsciiLower c || isAsciiUpper c || isDigit c
            || c == ':' || c == '.' || c == '_' || c == '-'

-- | Tally agent:thinker and agent:builder hits.
countEscalations :: [Text] -> Int
countEscalations agents =
    length [a | a <- agents, a == "agent:thinker" || a == "agent:builder"]

-- | Generic "find every occurrence of @prefix@ followed by a run of
-- characters satisfying @validChar@; return them with the prefix
-- preserved." Skips spurious matches (where the prefix is followed
-- by something non-matching) without infinite-looping.
findAllWithPrefix :: Text -> (Char -> Bool) -> Text -> [Text]
findAllWithPrefix prefix validChar = go
  where
    go t =
        case T.breakOn prefix t of
            (_, rest) | T.null rest -> []
            (_, rest) ->
                let after     = T.drop (T.length prefix) rest
                    extracted = T.takeWhile validChar after
                    -- Always advance at least past the prefix so we
                    -- don't loop on a no-match position.
                    advanceBy = max 1 (T.length extracted)
                    remainder = T.drop advanceBy after
                in  if T.null extracted
                        then go remainder
                        else (prefix <> extracted) : go remainder

-- | Build a descending-by-count association list from a flat list of
-- repeats.
countOccurrences :: Ord a => [a] -> [(a, Int)]
countOccurrences xs =
    sortBy (comparing (Down . snd))
        (Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs]))

-- === Orchestration ===========================================================

-- | Tail the log + run all extractors. Wrapped in 'Proc' for the
-- @sudo tail@ subprocess.
runSummary
    :: (Proc :> es, IOE :> es)
    => SummaryConfig -> Eff es SummaryReport
runSummary cfg = do
    path <- liftIO latestLogPath
    body <- fetchTail (scLineCount cfg) path
    let agents       = extractAgentRefs body
        models       = extractModelRefs body
        escalations  = countEscalations agents
    pure SummaryReport
        { srAgents      = countOccurrences agents
        , srModels      = countOccurrences models
        , srEscalations = escalations
        , srLogPath     = T.pack path
        , srLineCount   = scLineCount cfg
        }

-- | Path to today's OpenClaw detailed log. We compute this rather than
-- caching it so a long-running process picks up the rollover at
-- midnight without restart (not that we expect one, but cheap to do).
latestLogPath :: IO FilePath
latestLogPath = do
    today <- Time.utctDay <$> Time.getCurrentTime
    let stamp = Time.formatTime Time.defaultTimeLocale "%Y-%m-%d" today
    pure ("/tmp/openclaw/openclaw-" <> stamp <> ".log")

-- | @sudo tail -N path@. Returns empty text on subprocess failure
-- so the report still renders (with zero counts) -- caller knows
-- the path it was looking for and can investigate.
fetchTail :: (Proc :> es) => Int -> FilePath -> Eff es Text
fetchTail n path = do
    r <- runCmd "sudo" ["tail", "-" <> show n, path] (Seconds 10)
    pure $ case r of
        Right ProcResult { prStdout = out } -> out
        Left _                              -> ""

-- === Reporting ===============================================================

-- | Plain-text report matching the bash version's shape.
renderReport :: SummaryReport -> Text
renderReport r =
    T.intercalate "\n" $
        [ banner "agent session keys (last " (srLineCount r) " events)"
        , renderCountTable (srAgents r)
        , ""
        , banner "model references (last " (srLineCount r) " events)"
        , renderCountTable (srModels r)
        , ""
        , "== escalations to thinker/builder: " <> T.pack (show (srEscalations r)) <> " =="
        ]
  where
    banner pre n suf =
        "== " <> pre <> T.pack (show n) <> suf <> " =="

renderCountTable :: [(Text, Int)] -> Text
renderCountTable [] = "  (none)"
renderCountTable rows =
    T.intercalate "\n" (map row rows)
  where
    row (label, n) =
        "  " <> T.justifyRight 6 ' ' (T.pack (show n)) <> "  " <> label
