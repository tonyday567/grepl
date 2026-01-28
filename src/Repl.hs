module Repl
  ( startRepl
  , queryRepl
  ) where

import System.Process
import System.IO
import Control.Concurrent (threadDelay, forkIO)
import Data.List (isInfixOf)

-- | Start a cabal repl session with file-based stdio.
--
-- Spawns 'cabal repl' in the given project directory.
-- Wires stdin from /tmp/ghci-in.txt (via pipe + thread)
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

  -- Create pipes for stdin (cabal repl will never see EOF)
  (inReadHandle, inWriteHandle) <- createPipe

  -- Open output files
  outHandle <- openFile "/tmp/ghci-out.txt" WriteMode
  errHandle <- openFile "/tmp/ghci-err.txt" WriteMode

  -- Set line buffering so output appears immediately
  hSetBuffering outHandle LineBuffering
  hSetBuffering errHandle LineBuffering
  hSetBuffering inWriteHandle LineBuffering

  -- Spawn a thread that reads from /tmp/ghci-in.txt and writes to the pipe
  _ <- forkIO $ inputThread inWriteHandle

  -- Create the process specification
  let procSpec = (shell "cabal repl")
        { cwd = Just projectDir
        , std_in = UseHandle inReadHandle
        , std_out = UseHandle outHandle
        , std_err = UseHandle errHandle
        }

  -- Spawn the process
  (_, _, _, ph) <- createProcess procSpec

  return ph

-- | Thread that reads from /tmp/ghci-in.txt and writes to the stdin pipe.
-- Polls the input file for new lines and forwards them to the process.
inputThread :: Handle -> IO ()
inputThread writeHandle = loop 0
  where
    loop linesSeen = do
      threadDelay 50000  -- 50ms polling interval

      -- Read current input file
      input <- readFile "/tmp/ghci-in.txt"
      let currentLines = lines input
      let newLines = drop linesSeen currentLines

      -- Write any new lines to the pipe
      mapM_ (\line -> hPutStrLn writeHandle line) newLines

      -- Continue with updated line count
      loop (length currentLines)

-- | Query the running REPL instance.
--
-- Appends the query to /tmp/ghci-in.txt, then polls /tmp/ghci-out.txt
-- for a response matching the expected pattern.
--
-- Returns Just the response if found within 3 seconds, Nothing on timeout.
queryRepl :: String -> String -> IO (Maybe String)
queryRepl query expectPattern = do
  -- Count lines in output file before query
  outputBefore <- readFile "/tmp/ghci-out.txt"
  let linesBefore = length (lines outputBefore)

  -- Append query to input file
  appendFile "/tmp/ghci-in.txt" (query ++ "\n")

  -- Poll for response (60 attempts, 50ms each = 3 seconds)
  tryUntilFound 60 linesBefore

  where
    tryUntilFound :: Int -> Int -> IO (Maybe String)
    tryUntilFound 0 _ = return Nothing
    tryUntilFound attempts linesBefore = do
      threadDelay 50000  -- 50ms

      -- Read current output
      output <- readFile "/tmp/ghci-out.txt"
      let currentLines = lines output
      let linesAfter = drop linesBefore currentLines

      -- Check if we have new lines with the expected pattern
      case filter (expectPattern `isInfixOf`) linesAfter of
        [] -> tryUntilFound (attempts - 1) linesBefore  -- Not found yet
        matches -> return $ Just (unlines matches)  -- Found it
