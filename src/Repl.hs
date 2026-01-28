module Repl
  ( startRepl
  , queryRepl
  ) where

import System.Process
import System.IO
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race, async)
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

  -- Set no buffering for immediate output
  hSetBuffering outHandle NoBuffering
  hSetBuffering errHandle NoBuffering
  hSetBuffering inWriteHandle LineBuffering

  -- Spawn single input watcher thread (using async)
  _ <- async (inputWatcher inWriteHandle)

  -- Create the process specification
  let procSpec = (shell "cabal repl")
        { cwd = Just projectDir
        , std_in = UseHandle inReadHandle
        , std_out = UseHandle outHandle
        , std_err = UseHandle errHandle
        }

  -- Spawn the process
  (_, _, _, ph) <- createProcess procSpec

  -- Note: Keep handles open so process can write throughout its lifetime

  return ph

-- | Single input watcher thread.
-- Reads from /tmp/ghci-in.txt and writes to the stdin pipe.
-- Runs continuously, polling input file at relaxed 500ms intervals.
inputWatcher :: Handle -> IO ()
inputWatcher writeHandle = loop 0
  where
    loop linesSeen = do
      threadDelay 500000  -- 500ms (reduced polling, less contention)

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
-- Appends the query to /tmp/ghci-in.txt, then waits for a response
-- matching the expected pattern using race (3-second timeout).
--
-- Returns Just the response if found within 3 seconds, Nothing on timeout.
queryRepl :: String -> String -> IO (Maybe String)
queryRepl query expectPattern = do
  -- Get baseline line count
  outputBefore <- readFile "/tmp/ghci-out.txt"
  let linesBefore = length (lines outputBefore)

  -- Append query to input file (input watcher will see it)
  appendFile "/tmp/ghci-in.txt" (query ++ "\n")

  -- Race: timeout vs wait for response pattern
  result <- race
    (threadDelay 3000000)  -- 3 second timeout
    (waitForPattern linesBefore expectPattern)

  case result of
    Left () -> return Nothing           -- timeout
    Right resp -> return (Just resp)    -- got response

-- | Wait for expected pattern in output file.
-- Polls output file looking for new lines containing the pattern.
waitForPattern :: Int -> String -> IO String
waitForPattern linesBefore expectPattern = loop 0
  where
    loop attempts
      | attempts > 60 = fail "waitForPattern: max attempts exceeded"
      | otherwise = do
          threadDelay 50000  -- 50ms poll

          -- Read current output (plain read should work with keep-open handles)
          output <- readFile "/tmp/ghci-out.txt"
          let allLines = lines output
          let newLines = drop linesBefore allLines

          -- Check for pattern in new lines
          case filter (expectPattern `isInfixOf`) newLines of
            [] -> loop (attempts + 1)         -- Not found yet, keep polling
            matches -> return (unlines matches) -- Found it

-- | Strictly read a file (forces immediate evaluation, sees fresh content)
readFileStrict :: FilePath -> IO String
readFileStrict path = withFile path ReadMode $ \h -> do
  content <- hGetContents h
  length content `seq` return content
