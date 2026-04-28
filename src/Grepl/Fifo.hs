-- |
-- Module      : Grepl.Fifo
-- Copyright   : (c) 2026 Tony Day
-- License     : BSD-3-Clause
-- Maintainer  : tonyday567@gmail.com
--
-- File-based message passing protocol for querying GHCi instances using named pipes (FIFOs).
--
-- = Overview
--
-- @Grepl.Fifo@ abstracts cabal-repl process management for agentic workflows.
-- It uses named pipes (FIFOs) to decouple process I/O, enabling reliable
-- interaction with console applications in stateful, asynchronous agent contexts.
--
-- = Usage
--
-- Spawn a cabal-repl session with default configuration:
--
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
module Grepl.Fifo
  ( ChannelConfig (..),
    defaultChannelConfig,
    exeChannelConfig,
    channel,
  )
where

import Control.Monad (unless)
import System.Directory (doesFileExist)
import System.IO
import System.Process

-- | Configuration for the cabal-repl channel
--
-- Specifies process command, working directory, and named pipe paths.
--
-- >>> defaultChannelConfig
-- ChannelConfig {processCommand = "cabal repl", projectDir = ".", stdinPath = "/tmp/ghci-in", stdoutPath = "./log/cabal-repl-stdout.md", stderrPath = "./log/cabal-repl-stderr.md"}
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
--
-- Runs @cabal repl@ with pipes in /tmp and logs in ./log/
--
-- Suitable for library REPL sessions in the current project.
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
--
-- Runs @cabal repl grepl-explore@ with separate pipe paths.
--
-- Suitable for executable target REPL sessions; uses different pipe names
-- to avoid conflicts with 'defaultChannelConfig'.
exeChannelConfig :: ChannelConfig
exeChannelConfig =
  ChannelConfig
    { processCommand = "cabal repl grepl-explore",
      projectDir = ".",
      stdinPath = "/tmp/ghci-in-exe",
      stdoutPath = "./log/cabal-repl-exe-stdout.md",
      stderrPath = "./log/cabal-repl-exe-stderr.md"
    }

-- | Ensure a FIFO exists, creating it if necessary via @mkfifo@
--
-- Idempotent: calling multiple times on the same path is safe.
ensureFifo :: FilePath -> IO ()
ensureFifo path = do
  exists <- doesFileExist path
  unless exists $ do
    callProcess "mkfifo" [path]

-- | Start a cabal repl session with named pipes
--
-- Creates the stdin FIFO if it doesn't exist, opens log files for appending,
-- and spawns the process with no buffering on output handles.
--
-- Returns a 'ProcessHandle' for the running cabal repl process.
--
-- The process reads queries from the stdin FIFO and logs output to markdown files.
-- Agents write queries via 'System.IO.writeFile' to 'stdinPath' and read results
-- from the logged files as they accumulate.
--
-- Example: Start default library REPL
--
-- > ph <- channel defaultChannelConfig
-- > -- Now GHCi is listening on /tmp/ghci-in for queries
-- > -- Results appear in ./log/cabal-repl-stdout.md and ./log/cabal-repl-stderr.md
--
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
