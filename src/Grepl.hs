-- |
-- Module      : Grepl
-- Copyright   : (c) 2026 Tony Day
-- License     : BSD-3-Clause
-- Maintainer  : tonyday567@gmail.com
--
-- File-based message passing protocol for querying GHCi instances.
--
-- = Overview
--
-- @Grepl@ abstracts cabal-repl process management for agentic workflows.
-- It uses named pipes (FIFOs) to decouple process I/O, enabling reliable
-- interaction with console applications in stateful, asynchronous agent contexts.
--
-- = Usage
--
-- Spawn a cabal-repl session with default configuration:
--
-- > import Grepl
-- > ph <- channel defaultChannelConfig
--
-- Or use custom configuration:
--
-- > let cfg = ChannelConfig
-- >       { processCommand = "cabal repl"
-- >       , projectDir = "."
-- >       , stdinPath = "/tmp/ghci-in"
-- >       , stdoutPath = "./log/cabal-repl-stdout.md"
-- >       , stderrPath = "./log/cabal-repl-stderr.md"
-- >       }
-- > ph <- channel cfg
--
-- = Design
--
-- Named pipes provide stable I/O decoupling for agent workflows:
--
-- - Agents write to stdin FIFO without blocking on console buffering
-- - Stdout and stderr are logged to files for inspection and history
-- - Process lifecycle is independent of I/O, supporting multiplexing
--
-- See "Grepl.Watcher" for watching markdown log files in response to agent queries.
module Grepl
  ( ChannelConfig (..),
    defaultChannelConfig,
    exeChannelConfig,
    channel,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, race)
import Control.Monad (unless)
import Data.List (isInfixOf)
import System.Directory (doesFileExist)
import System.IO
import System.Process

-- | Configuration for the cabal-repl channel
data ChannelConfig = ChannelConfig
  { -- | Command to run (e.g., "cabal repl")
    processCommand :: String,
    -- | Project directory (where .cabal file lives)
    projectDir :: FilePath,
    -- | Path to stdin FIFO
    stdinPath :: FilePath,
    -- | Path to stdout log file
    stdoutPath :: FilePath,
    -- | Path to stderr log file
    stderrPath :: FilePath
  }
  deriving (Show, Eq)

-- | Default channel configuration
defaultChannelConfig :: ChannelConfig
defaultChannelConfig =
  ChannelConfig
    { processCommand = "cabal repl",
      projectDir = ".",
      stdinPath = "/tmp/ghci-in",
      stdoutPath = "./log/cabal-repl-stdout.md",
      stderrPath = "./log/cabal-repl-stderr.md"
    }

-- | Executable channel configuration
exeChannelConfig :: ChannelConfig
exeChannelConfig =
  ChannelConfig
    { processCommand = "cabal repl grepl-explore",
      projectDir = ".",
      stdinPath = "/tmp/ghci-in-exe",
      stdoutPath = "./log/cabal-repl-exe-stdout.md",
      stderrPath = "./log/cabal-repl-exe-stderr.md"
    }

-- | Ensure a FIFO exists, creating it if necessary
ensureFifo :: FilePath -> IO ()
ensureFifo path = do
  exists <- doesFileExist path
  unless exists $ do
    callProcess "mkfifo" [path]

-- | Start a cabal repl session with named pipes
--
-- Creates stdin FIFO if it doesn't exist.
-- Opens handles for stdin (FIFO), stdout, and stderr (append mode).
-- Spawns the process with those handles wired.
-- Returns a ProcessHandle to the running process.
channel :: ChannelConfig -> IO ProcessHandle
channel cfg = do
  -- Create stdin FIFO if it doesn't exist
  ensureFifo (stdinPath cfg)

  -- Open stdin FIFO for reading
  stdinHandle <- openFile (stdinPath cfg) ReadMode

  -- Open stdout and stderr for appending
  stdoutHandle <- openFile (stdoutPath cfg) AppendMode
  stderrHandle <- openFile (stderrPath cfg) AppendMode

  -- Set no buffering for immediate output
  hSetBuffering stdoutHandle NoBuffering
  hSetBuffering stderrHandle NoBuffering

  -- Create the process specification
  let procSpec =
        (shell (processCommand cfg))
          { cwd = Just (projectDir cfg),
            std_in = UseHandle stdinHandle,
            std_out = UseHandle stdoutHandle,
            std_err = UseHandle stderrHandle
          }

  -- Spawn the process
  (_, _, _, ph) <- createProcess procSpec

  return ph
