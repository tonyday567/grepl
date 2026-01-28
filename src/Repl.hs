module Repl
  ( startRepl
  , queryRepl
  ) where

import System.Process
import System.IO
import Control.Concurrent (threadDelay)
import Data.List (isInfixOf)

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

-- | Query the running REPL instance.
--
-- Appends the query to /tmp/ghci-in.txt, then polls /tmp/ghci-out.txt
-- for a response matching the expected pattern.
--
-- Returns Just the response if found within 2 seconds, Nothing on timeout.
queryRepl :: String -> String -> IO (Maybe String)
queryRepl query expectPattern = do
  -- Count lines in output file before query
  outputBefore <- readFile "/tmp/ghci-out.txt"
  let linesBefore = length (lines outputBefore)

  -- Append query to input file
  appendFile "/tmp/ghci-in.txt" (query ++ "\n")

  -- Poll for response (20 attempts, 100ms each = 2 seconds)
  tryUntilFound 20 linesBefore

  where
    tryUntilFound :: Int -> Int -> IO (Maybe String)
    tryUntilFound 0 _ = return Nothing
    tryUntilFound attempts linesBefore = do
      threadDelay 100000  -- 100ms

      -- Read current output
      output <- readFile "/tmp/ghci-out.txt"
      let currentLines = lines output
      let linesAfter = drop linesBefore currentLines

      -- Check if we have new lines with the expected pattern
      case filter (expectPattern `isInfixOf`) linesAfter of
        [] -> tryUntilFound (attempts - 1) linesBefore  -- Not found yet
        matches -> return $ Just (unlines matches)  -- Found it
