{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Grepl
import Grepl.Watcher
import Data.List (intercalate, nub)
import Data.Time (getCurrentTime)
import GHC.Generics
import Options.Applicative
import Options.Applicative.Help.Pretty
import Perf
import Prelude
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import System.IO
import Control.Concurrent
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (TChan, atomically, readTChan)
import System.Timeout (timeout)
import System.FilePath (takeDirectory)

data Run = RunChannel | RunChannelExe | RunBenchmark | RunWatcher deriving (Eq, Show)

data AppConfig = AppConfig
  { appRun :: Run,
    keepAliveSeconds :: Int,
    appReportOptions :: ReportOptions
  }
  deriving (Eq, Show, Generic)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig RunChannel 600 defaultReportOptions

parseRun :: Parser Run
parseRun =
  flag' RunChannel (long "channel" <> help "run default channel (default)")
    <|> flag' RunChannelExe (long "exe" <> help "run exe channel config")
    <|> flag' RunBenchmark (long "benchmark" <> help "run benchmark mode")
    <|> flag' RunWatcher (long "watch" <> help "watch log directory for changes (debug)")
    <|> pure RunChannel

appParser :: AppConfig -> Parser AppConfig
appParser def =
  AppConfig
    <$> parseRun
    <*> option auto
        (long "keepAlive"
          <> short 'k'
          <> value (keepAliveSeconds def)
          <> help "How long to keep channel alive in seconds (default: 600)")
    <*> parseReportOptions (appReportOptions def)

appConfig :: AppConfig -> ParserInfo AppConfig
appConfig def =
  info
    (appParser def <**> helper)
    (fullDesc <> progDesc "Interactive GHCi channel via named pipes" <> header "repl-explore - test harness for cabal-repl channel")

-- | Separate channel config for benchmarking (isolated from default channel)
benchChannelConfig :: ChannelConfig
benchChannelConfig = ChannelConfig
  { processCommand = "cabal repl"
  , projectDir = "."
  , stdinPath = "/tmp/ghci-bench-in"
  , stdoutPath = "./log/cabal-repl-bench-stdout.md"
  , stderrPath = "./log/cabal-repl-bench-stderr.md"
  }

main :: IO ()
main = do
  config <- execParser (appConfig defaultAppConfig)
  
  case appRun config of
    RunChannel -> runChannel (keepAliveSeconds config) defaultChannelConfig
    RunChannelExe -> runChannel (keepAliveSeconds config) exeChannelConfig
    RunBenchmark -> runBenchmark (appReportOptions config)
    RunWatcher -> runWatcher

runChannel :: Int -> ChannelConfig -> IO ()
runChannel keepAlive cfg = do
  hPutStrLn stderr $ "Starting channel with keepAlive=" ++ show keepAlive ++ "s..."
  
  h <- channel cfg
  hPutStrLn stderr "✓ Channel started"
  
  let waitMs = keepAlive * 1000000
  threadDelay waitMs
  
  hPutStrLn stderr "Done."

runBenchmark :: ReportOptions -> IO ()
runBenchmark repOptions = do
  hPutStrLn stderr "Starting benchmark (channel I/O latency)..."
  
  -- Set up channel and watcher in separate async threads
  _ <- async $ channel benchChannelConfig
  hPutStrLn stderr "✓ Channel started"
  
  let logDir = takeDirectory (stdoutPath benchChannelConfig)
  hPutStrLn stderr $ "  Watching directory: " ++ logDir
  hPutStrLn stderr $ "  Log file: " ++ stdoutPath benchChannelConfig
  
  outChan <- watchMarkdown logDir
  hPutStrLn stderr "✓ Watcher started"
  
  -- Give everything time to settle
  threadDelay 1000000
  
  -- Run the benchmark (single iteration, \_ ignores length parameter)
  reportMain repOptions "grepl-channel-latency" $ \_ -> benchmarkChannelLatency outChan

-- | Benchmark: measure latency from writing query to receiving file change signal
-- Ignores length param (single run per invocation by reportMain)
benchmarkChannelLatency :: TChan String -> PerfT IO [[Double]] ()
benchmarkChannelLatency outChan = do
  let inPath = stdinPath benchChannelConfig
  
  -- Write query to input FIFO
  liftIO $ do
    inHandle <- openFile inPath WriteMode
    hPutStrLn inHandle ":t id"
    hFlush inHandle
    hClose inHandle
  
  -- Wait for watcher to signal file change (timeout after 3 seconds)
  mResult <- liftIO $ timeout 3000000 $ atomically $ readTChan outChan
  case mResult of
    Just _fp -> do
      liftIO $ hPutStrLn stderr $ "  ✓ Signal received: " ++ show _fp
      pure ()
    Nothing -> do
      liftIO $ hPutStrLn stderr $ "  ✗ Timeout waiting for watcher signal"
      pure ()

-- | Debug mode: watch log directory and print file change events
runWatcher :: IO ()
runWatcher = do
  hPutStrLn stderr "Starting watcher on ./log/ directory..."
  
  outChan <- watchMarkdown "./log"
  hPutStrLn stderr "✓ Watcher started"
  
  hPutStrLn stderr "Watching for .md file changes. Press Ctrl-C to stop."
  hPutStrLn stderr ""
  
  -- Print every file change event
  forever $ do
    fp <- atomically $ readTChan outChan
    getCurrentTime >>= \now -> hPutStrLn stderr $ "[" ++ show now ++ "] Modified: " ++ fp
