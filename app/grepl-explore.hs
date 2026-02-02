{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Grepl
import Data.List (intercalate, nub)
import Data.Time (getCurrentTime, diffUTCTime)
import GHC.Generics
import Options.Applicative
import Options.Applicative.Help.Pretty
import Perf
import Prelude
import Control.Monad
import System.IO
import Control.Concurrent
import qualified Data.Text.IO as TIO

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
  
  -- Start channel in background
  h <- channel defaultChannelConfig
  hPutStrLn stderr "✓ Channel started"
  
  -- Give GHCi time to settle
  threadDelay 1000000
  
  -- Run the benchmark
  reportMain repOptions "grepl-channel-latency" benchmarkChannelLatency

-- | Benchmark: measure latency of writing a query and polling for response
-- Int param: number of queries to send
benchmarkChannelLatency :: Int -> PerfT IO [[Double]] ()
benchmarkChannelLatency n = do
  let inPath = stdinPath defaultChannelConfig
  let outPath = stdoutPath defaultChannelConfig
  
  -- Send n queries and measure total time
  forM_ [1..n] $ \i -> do
    let query = ":t id -- q" ++ show i ++ "\n"
    let marker = "-- q" ++ show i
    
    -- Write query to FIFO
    liftIO $ do
      inHandle <- openFile inPath WriteMode
      hPutStr inHandle query
      hFlush inHandle
      hClose inHandle
    
    -- Poll for response (up to 5 seconds, 10ms intervals)
    liftIO $ pollForMarker outPath marker 500 10000

-- | Poll output file for a marker string, return when found or timeout
pollForMarker :: FilePath -> String -> Int -> Int -> IO ()
pollForMarker outPath marker maxPolls pollIntervalUs = loop 0
  where
    loop pollCount
      | pollCount >= maxPolls = do
          hPutStrLn stderr $ "✗ Timeout waiting for: " ++ marker
      | otherwise = do
          content <- TIO.readFile outPath
          if marker `isInfixOf` content
            then pure ()
            else do
              threadDelay pollIntervalUs
              loop (pollCount + 1)
    
    -- Simple substring search
    isInfixOf needle haystack = 
      needle `isSubstringOf` haystack
    
    isSubstringOf [] _ = True
    isSubstringOf _ [] = False
    isSubstringOf s@(x:xs) (y:ys)
      | x == y = isSubstringOf xs ys
      | otherwise = isSubstringOf s ys
