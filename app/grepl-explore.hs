{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Control.Concurrent
import Control.Concurrent.Async (async)
import Control.Concurrent.STM (TChan, atomically, readTChan)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, nub)
import Data.Time (getCurrentTime)
import GHC.Generics
import Grepl
import Grepl.Watcher
import Options.Applicative
import Options.Applicative.Help.Pretty
import Perf
import System.FilePath (takeDirectory)
import System.IO
import System.Timeout (timeout)
import Prelude

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
    <*> option
      auto
      ( long "keepAlive"
          <> short 'k'
          <> value (keepAliveSeconds def)
          <> help "How long to keep channel alive in seconds (default: 600)"
      )
    <*> parseReportOptions (appReportOptions def)

appConfig :: AppConfig -> ParserInfo AppConfig
appConfig def =
  info
    (appParser def <**> helper)
    (fullDesc <> progDesc "Interactive GHCi channel via named pipes" <> header "repl-explore - test harness for cabal-repl channel")

-- | Separate channel config for benchmarking (isolated from default channel)
benchChannelConfig :: ChannelConfig
benchChannelConfig =
  ChannelConfig
    { processCommand = "cabal repl",
      projectDir = ".",
      stdinPath = "/tmp/ghci-bench-in",
      stdoutPath = "./log/cabal-repl-bench-stdout.md",
      stderrPath = "./log/cabal-repl-bench-stderr.md"
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
  -- Set up channel and watcher in separate async threads
  _ <- async $ channel benchChannelConfig

  let logDir = takeDirectory (stdoutPath benchChannelConfig)
  outChan <- watchMarkdown logDir

  -- Give everything time to settle
  threadDelay 1000000

  -- Run the benchmark
  reportMain repOptions "grepl-channel-latency" (\_ -> benchmarkChannelThroughput outChan)

-- | Benchmark: measure latency from writing query to receiving file change signal
benchmarkChannelThroughput :: (Semigroup t) => TChan String -> PerfT IO t ()
benchmarkChannelThroughput outChan = do
  let inPath = stdinPath benchChannelConfig

  _ <- fam "timeout" $ do
    threadDelay 1000000
    hPutStrLn stderr "timeout"

  _ <- fam "timeout2" $ do
    threadDelay 2000000
    hPutStrLn stderr "timeout"

  -- Write query to input FIFO
  inh <- fam "file write" $ do
    inHandle <- openFile inPath WriteMode
    hPutStrLn inHandle ":t id"
    hFlush inHandle
    liftIO (pure inHandle)

  -- Wait for watcher to signal file change (timeout after 3 seconds)
  _ <- fam "query signal" $ do
    mResult <- timeout 3000000 $ atomically $ readTChan outChan
    hPutStrLn stderr $ "Signal 1: " ++ show mResult

  liftIO $ threadDelay 1000000

  -- Wait for watcher to signal file change (timeout after 3 seconds)
  _ <- fam "reponse signal" $ do
    mResult <- timeout 3000000 $ atomically $ readTChan outChan
    hPutStrLn stderr $ "Signal 2: " ++ show mResult

  liftIO $ do
    hClose inh
    putStrLn "closed input channel"
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
