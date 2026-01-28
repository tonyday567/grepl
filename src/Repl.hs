module Repl
  ( startRepl
  ) where

import System.Process
import System.IO

-- | Start a cabal repl session with file-based stdio.
--
-- Spawns 'cabal repl' in the given project directory.
-- Wires stdin from /tmp/ghci-in.txt
-- Wires stdout to /tmp/ghci-out.txt
-- Wires stderr to /tmp/ghci-err.txt
--
-- Returns a ProcessHandle to the running process.
startRepl :: FilePath -> IO ProcessHandle
startRepl projectDir = do
  -- Initialize the communication files
  writeFile "/tmp/ghci-in.txt" ""
  writeFile "/tmp/ghci-out.txt" ""
  writeFile "/tmp/ghci-err.txt" ""

  -- Open file handles
  inHandle <- openFile "/tmp/ghci-in.txt" ReadMode
  outHandle <- openFile "/tmp/ghci-out.txt" WriteMode
  errHandle <- openFile "/tmp/ghci-err.txt" WriteMode

  -- Set line buffering so output appears immediately
  hSetBuffering outHandle LineBuffering
  hSetBuffering errHandle LineBuffering
  hSetBuffering inHandle LineBuffering

  -- Create the process specification
  let procSpec = (shell "cabal repl")
        { cwd = Just projectDir
        , std_in = UseHandle inHandle
        , std_out = UseHandle outHandle
        , std_err = UseHandle errHandle
        }

  -- Spawn the process
  (_, _, _, ph) <- createProcess procSpec

  return ph
