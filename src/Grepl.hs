module Grepl
  ( ChannelConfig (..)
  , defaultChannelConfig
  , exeChannelConfig
  , channel
  ) where

import System.Process
import System.IO
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race, async)
import Data.List (isInfixOf)

-- | Configuration for the cabal-repl channel
data ChannelConfig = ChannelConfig
  { processCommand :: String      -- ^ Command to run (e.g., "cabal repl")
  , projectDir :: FilePath        -- ^ Project directory (where .cabal file lives)
  , stdinPath :: FilePath         -- ^ Path to stdin FIFO
  , stdoutPath :: FilePath        -- ^ Path to stdout log file
  , stderrPath :: FilePath        -- ^ Path to stderr log file
  }
  deriving (Show, Eq)

-- | Default channel configuration
defaultChannelConfig :: ChannelConfig
defaultChannelConfig = ChannelConfig
  { processCommand = "cabal repl"
  , projectDir = "."
  , stdinPath = "/tmp/ghci-in"
  , stdoutPath = "./log/cabal-repl-stdout.md"
  , stderrPath = "./log/cabal-repl-stderr.md"
  }

-- | Executable channel configuration
exeChannelConfig :: ChannelConfig
exeChannelConfig = ChannelConfig
  { processCommand = "cabal repl grepl-explore"
  , projectDir = "."
  , stdinPath = "/tmp/ghci-in-exe"
  , stdoutPath = "./log/cabal-repl-exe-stdout.md"
  , stderrPath = "./log/cabal-repl-exe-stderr.md"
  }

-- | Start a cabal repl session with named pipes
--
-- Opens handles for stdin (FIFO), stdout, and stderr (append mode).
-- Spawns the process with those handles wired.
-- Returns a ProcessHandle to the running process.
channel :: ChannelConfig -> IO ProcessHandle
channel cfg = do
  -- Open stdin FIFO for reading
  stdinHandle <- openFile (stdinPath cfg) ReadMode
  
  -- Open stdout and stderr for appending
  stdoutHandle <- openFile (stdoutPath cfg) AppendMode
  stderrHandle <- openFile (stderrPath cfg) AppendMode
  
  -- Set no buffering for immediate output
  hSetBuffering stdoutHandle NoBuffering
  hSetBuffering stderrHandle NoBuffering
  
  -- Create the process specification
  let procSpec = (shell (processCommand cfg))
        { cwd = Just (projectDir cfg)
        , std_in = UseHandle stdinHandle
        , std_out = UseHandle stdoutHandle
        , std_err = UseHandle stderrHandle
        }
  
  -- Spawn the process
  (_, _, _, ph) <- createProcess procSpec
  
  return ph


