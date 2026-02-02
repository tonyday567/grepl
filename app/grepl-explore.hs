{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Grepl
import Grepl.Watcher
import Data.List (intercalate, nub)
import GHC.Generics
import Options.Applicative
import Options.Applicative.Help.Pretty
import Perf
import Prelude
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import System.IO
import Control.Concurrent
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (TChan, atomically, readTChan)
import System.FilePath (takeDirectory)

data Run = RunChannel | RunChannelExe | RunBenchmark deriving (Eq, Show)

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
  
  -- Wait for watcher to signal file change (blocks until TChan receives)
  _ <- liftIO $ atomically $ readTChan outChan
  pure ()
