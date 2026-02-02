{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | basic measurement and callibration
module Main where

import Grepl
import Data.List (intercalate, nub)
import GHC.Generics
import Options.Applicative
import Options.Applicative.Help.Pretty
import Perf
import Prelude
import Control.Monad
import System.IO
import Control.Concurrent

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
    RunBenchmark -> runBenchmark

runChannel :: Int -> ChannelConfig -> IO ()
runChannel keepAlive cfg = do
  hPutStrLn stderr $ "Starting channel with keepAlive=" ++ show keepAlive ++ "s..."
  
  h <- channel cfg
  hPutStrLn stderr "✓ Channel started"
  
  let waitMs = keepAlive * 1000000
  threadDelay waitMs
  
  hPutStrLn stderr "Done."

runBenchmark :: IO ()
runBenchmark = do
  hPutStrLn stderr "Benchmark mode (placeholder)"
