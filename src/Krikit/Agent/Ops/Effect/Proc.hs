{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Subprocess execution with bounded wall-clock time.
--
-- The real handler uses 'typed-process' + 'System.Timeout'. Tests swap
-- in 'runProcMock' with canned responses so tier logic is testable
-- without launching real commands.
module Krikit.Agent.Ops.Effect.Proc
    ( -- * Effect
      Proc
    , ProcError (..)
    , ProcResult (..)

      -- * Smart constructors
    , runCmd
    , runCmdAsUser

      -- * Handlers
    , runProcIO
    , runProcMock

      -- * Mock support
    , MockResponse
    , mockResponse
    ) where

import           Control.Exception           (SomeException, try)
import qualified Data.ByteString.Lazy        as LBS
import           Data.ByteString.Lazy        (ByteString)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as TE
import qualified Data.Text.Encoding.Error    as TE
import qualified Data.Time                   as Time
import           System.Exit                 (ExitCode (..))
import           System.Timeout              (timeout)

import           Effectful                   (Eff, Effect, IOE, liftIO, (:>))
import           Effectful.Dispatch.Dynamic  (interpret)
import           Effectful.TH                (makeEffect)

import qualified System.Process.Typed        as TP

-- | Why a subprocess invocation failed.
data ProcError
    = ProcTimeout             -- ^ wall-clock ceiling exceeded
    | ProcNonZero   !Int      -- ^ process exited with non-zero code
    | ProcLaunchErr !Text     -- ^ couldn't even start (file not found, permission, ...)
    deriving stock (Eq, Show)

-- | What came back from a successful subprocess.
data ProcResult = ProcResult
    { prStdout    :: !Text
    , prStderr    :: !Text
    , prElapsedMs :: !Int
    }
    deriving stock (Eq, Show)

data Proc :: Effect where
    -- | Run a command directly with args, bounded by N seconds.
    RunCmd       :: FilePath -> [String] -> Int
                 -> Proc m (Either ProcError ProcResult)

    -- | Run as another user via @sudo -u USER -i CMD...@. "-i" is a
    -- login shell so the target user's PATH/profile apply.
    RunCmdAsUser :: Text -> FilePath -> [String] -> Int
                 -> Proc m (Either ProcError ProcResult)

makeEffect ''Proc

-- | Real handler: runs via typed-process with a System.Timeout cap.
runProcIO :: IOE :> es => Eff (Proc : es) a -> Eff es a
runProcIO = interpret $ \_ -> \case
    RunCmd cmd args to ->
        liftIO (runReal cmd args to)
    RunCmdAsUser user cmd args to ->
        liftIO $
            runReal "sudo" (["-u", T.unpack user, "-i", cmd] ++ args) to

runReal :: FilePath -> [String] -> Int -> IO (Either ProcError ProcResult)
runReal cmd args toSec = do
    started <- wallMs
    result  <- try @SomeException $
        timeout (toSec * 1_000_000) (TP.readProcess (TP.proc cmd args))
    ended   <- wallMs
    let elapsed = ended - started
    pure $ case result of
        Left e            -> Left (ProcLaunchErr (T.pack (show e)))
        Right Nothing     -> Left ProcTimeout
        Right (Just (code, out, err)) -> case code of
            ExitSuccess   -> Right (mkOk out err elapsed)
            ExitFailure n -> Left (ProcNonZero n)
  where
    mkOk out err elapsed =
        ProcResult
            { prStdout    = decodeLenient out
            , prStderr    = decodeLenient err
            , prElapsedMs = elapsed
            }

-- | Decode bytes to Text, replacing invalid UTF-8 with U+FFFD rather
-- than throwing. Subprocess output is overwhelmingly valid UTF-8 in our
-- ops context; we accept silent replacement over crash risk.
decodeLenient :: ByteString -> Text
decodeLenient = TE.decodeUtf8With TE.lenientDecode . LBS.toStrict

-- | Wall-clock in milliseconds. Good enough for elapsed-time bookkeeping;
-- no monotonic guarantees, which we don't need for human-readable reports.
wallMs :: IO Int
wallMs = do
    t <- Time.getCurrentTime
    let diffSeconds = Time.diffUTCTime t refEpoch
    pure (floor (diffSeconds * 1000))
  where
    refEpoch = Time.UTCTime (Time.fromGregorian 2020 1 1) 0

-- === Mock handler ===========================================================

-- | One canned response for the mock handler: keyed by command name (the
-- first arg to 'runCmd' / 'runCmdAsUser'), value is what to return.
type MockResponse = Either ProcError ProcResult

-- | Helper for building a canned successful response.
mockResponse :: Text -> Text -> Int -> MockResponse
mockResponse stdout_ stderr_ elapsed =
    Right (ProcResult stdout_ stderr_ elapsed)

-- | Mock handler: returns the response mapped to the command name.
--   Anything not in the map returns 'ProcLaunchErr "no mock for ..."'.
runProcMock
    :: Map FilePath MockResponse
    -> Eff (Proc : es) a
    -> Eff es a
runProcMock responses = interpret $ \_ -> \case
    RunCmd cmd _ _ ->
        pure (lookupMock cmd)
    RunCmdAsUser _ cmd _ _ ->
        pure (lookupMock cmd)
  where
    lookupMock k =
        case Map.lookup k responses of
            Just r  -> r
            Nothing -> Left (ProcLaunchErr ("no mock for " <> T.pack k))
