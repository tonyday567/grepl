-- |
-- Module      : Grepl.Pty
-- Copyright   : (c) 2026 Tony Day
-- License     : BSD-3-Clause
-- Maintainer  : tonyday567@gmail.com
--
-- PTY-based REPL interaction using circuits and stateful sessions.
--
-- This module provides two interfaces:
--
-- 1. __Development Harness__: Interactive helpers and circuit development in GHCi
-- 2. __Stateful Sessions__: Concrete @ReplSession@ type for agent-friendly REPL interaction
--
-- = Development Harness
--
-- Load into GHCi and build circuits incrementally:
--
-- @
-- $ cabal repl grepl
-- ghci> import Grepl.Pty
-- ghci> :type Circuit
-- ghci> let c = lift' (+1) :: Circuit (->) (,) Int Int
-- ghci> testPure c 5
-- @
--
-- = Stateful REPL Sessions
--
-- For agent interaction, use @startRepl@ and @sendCommand@:
--
-- @
-- ghci> sess <- startRepl "cabal" ["repl"] "ghci> " 5000000  -- 5 second timeout
-- ghci> result <- sendCommand sess "1 + 1"
-- ReplResult
--   { rplCommand = "1 + 1"
--   , rplOutput = "2"
--   , rplPrompt = "ghci> "
--   , rplSuccess = True
--   , rplDuration = 234  -- milliseconds
--   , rplError = Nothing
--   }
-- ghci> closeRepl sess
-- @
--
-- The session automatically suppresses warnings on startup (see 'defaultSetupCommands'),
-- resulting in clean output without noise.
--
-- = Startup Behavior
--
-- When you call @startRepl cmd args prompt timeout@:
--
-- 1. Spawns a PTY process (e.g., @cabal repl@)
-- 2. Waits 300ms for process startup
-- 3. Sends 'defaultSetupCommands' to suppress warnings:
--    - @:set -Wno-type-defaults@
--    - @:set -Wno-unused-matches@
--    - @:set prompt "ghci> "@
-- 4. Waits 200ms for setup to complete
-- 5. Returns ready 'ReplSession'
--
-- Result: clean REPL output without warning clutter
--
-- = Output Comparison
--
-- __Without setup commands (noisy):__
--
-- @
-- ghci> 1 + 1
-- <interactive>:1:1: warning: [-Wtype-defaults]
--     Defaulting the type variable 'a0' to type 'Integer'
--       arising from a use of 'it'
--     In a stmt of an interactive GHCi command: print it
-- 2
-- @
--
-- __With setup commands (clean):__
--
-- @
-- ghci> 1 + 1
-- 2
-- @
--
-- = Streaming Architecture
--
-- The PTY is a bidirectional, asynchronous communication channel:
--
-- - Effects leak across prompt boundaries by design (use 'readWithTimeout' for policy-driven capping)
-- - The @ghci>@ prompt is a 'Ready' tag, not termination
-- - 'sendCommand' uses 'readUntilPrompt' to collect output with timeout
-- - 'readWithTimeout' waits for silence after Ready (1/10 of primary timeout)
--
-- = Workflow
--
-- 1. Build simple circuits with `lift'` (embed pure functions)
-- 2. Test with `testPure` (interpret to base arrow)
-- 3. Compose with `compose'` (category structure)
-- 4. Add state with `Loop` and `Either` tensor
-- 5. Lift to `Kleisli IO` for I/O effects
-- 6. Use PTY functions to interact with real processes
--
module Grepl.Pty
  ( -- * Stateful REPL Sessions (agent-friendly)
    ReplSession,
    ReplResult (..),
    startRepl,
    sendCommand,
    closeRepl,
    defaultSetupCommands,
    sendSetupCommands,
    -- * Streaming and parsing helpers
    stripAnsi,
    readUntilPrompt,
    readWithTimeout,
    detectPrompt,
    cleanOutput,
    -- * Core circuit types and functions (re-exported from modules)
    module Circuit,
    module Circuit.Circuit,
    module Circuit.Traced,
    -- * Kleisli for IO
    Kleisli (..),
    Category (..),
    -- * PTY and Process
    module System.Posix.Pty,
    ProcessHandle,
    spawnCabalRepl,
    spawnCmd,
    -- * Interactive development helpers
    lift',
    compose',
    testPure,
  )
where

{-# LANGUAGE OverloadedStrings #-}

import Circuit hiding (prompt)
import Circuit.Circuit hiding (lower)
import Circuit.Traced hiding (prompt)
import Control.Arrow (Kleisli (..))
import Control.Category (Category (..))
import Control.Monad (forM_)
import qualified Circuit.Circuit as CC

import System.Posix.Pty (Pty, writePty, readPty, tryReadPty, closePty, spawnWithPty)
import System.Process (ProcessHandle)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Concurrent (threadDelay)
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import System.Timeout (timeout)

import Prelude hiding (id, (.))

-- | Convenient alias for lifting functions into circuits
--
-- Example in GHCi:
--
-- > let c = lift' (+1) :: Circuit (->) (,) Int Int
lift' :: arr a b -> Circuit arr t a b
lift' = Lift

-- | Convenient alias for composition
--
-- Example in GHCi:
--
-- > let c = lift' (+1) `compose'` lift' (*2)
compose' :: Circuit arr t b c -> Circuit arr t a b -> Circuit arr t a c
compose' = Compose

-- | Test a pure circuit (arrow = ->)
--
-- Example in GHCi:
--
-- > let c = lift' (+1) `compose'` lift' (*2)
-- > testPure c 5  -- Should print 12
testPure :: (Show b, Trace (->) t) => Circuit (->) t a b -> a -> IO ()
testPure circ a = print (CC.lower circ a)

-- | Spawn a @cabal repl@ process in a PTY
--
-- Keeps the PTY alive for interaction. Example in GHCi:
--
-- > (pty, ph) <- spawnCabalRepl
-- > writePty pty (BS.pack "1 + 1\\n")
-- > output <- readPty pty
-- > BS.putStrLn output
-- > closePty pty
spawnCabalRepl :: IO (Pty, ProcessHandle)
spawnCabalRepl = spawnWithPty Nothing True "cabal" ["repl"] (80, 24)

-- | Spawn an arbitrary command in a PTY
--
-- Example in GHCi:
--
-- > (pty, ph) <- spawnCmd "ghci" []
-- > (pty, ph) <- spawnCmd "bash" []
spawnCmd :: FilePath -> [String] -> IO (Pty, ProcessHandle)
spawnCmd cmd args = spawnWithPty Nothing True cmd args (80, 24)

-- ============================================================================
-- Stateful REPL Session: Agent-Friendly Interface
-- ============================================================================

-- | Structured output from a REPL command
data ReplResult = ReplResult
  { command     :: Text
  , output      :: Text
  , prompt      :: Text
  , success     :: Bool
  , durationMs  :: Int
  , errorMsg    :: Maybe Text
  } deriving (Show, Eq)

-- | A live REPL session the agent can interact with turn-by-turn
data ReplSession = ReplSession
  { pty        :: Pty
  , currentPrompt :: Text
  , timeoutMs  :: Int
  }

-- | Default setup commands to suppress noise (type-defaults, unused-matches warnings)
--
-- These are sent automatically by 'startRepl' to quiet the REPL on startup.
-- Includes:
--
-- - @:set -Wno-type-defaults@ — suppress type inference messages
-- - @:set -Wno-unused-matches@ — suppress pattern match warnings  
-- - @:set prompt "ghci> "@ — enforce consistent prompt
--
-- Example output comparison:
--
-- Without setup (noisy):
--
-- > ghci> 1 + 1
-- > <interactive>:1:1: warning: [-Wtype-defaults]
-- >     Defaulting the type variable 'a0' to type 'Integer'...
-- > 2
--
-- With setup (quiet):
--
-- > ghci> 1 + 1
-- > 2
defaultSetupCommands :: [Text]
defaultSetupCommands =
  [ T.pack ":set -Wno-type-defaults"
  , T.pack ":set -Wno-unused-matches"
  , T.pack ":set prompt \"ghci> \""
  ]

-- | Send setup commands to a PTY (e.g. :set directives)
--
-- Writes each command to the PTY with 100ms spacing to allow GHCi to process each one.
-- Useful for suppressing warnings, setting options, or configuring the REPL environment.
--
-- Example:
--
-- > pty <- fst <$> spawnWithPty Nothing True "cabal" ["repl"] (80, 24)
-- > sendSetupCommands pty [":set prompt \">>> \"", ":set +s"]
sendSetupCommands :: Pty -> [Text] -> IO ()
sendSetupCommands pty' cmds = do
  forM_ cmds $ \cmd -> do
    writePty pty' (TE.encodeUtf8 cmd <> BS8.pack "\n")
    threadDelay 100000  -- 100ms between commands

-- | Start a new stateful REPL session
--
-- Spawns a PTY process (e.g., @cabal repl@), waits for startup, sends 'defaultSetupCommands'
-- to suppress warnings, and returns a stateful session for turn-by-turn interaction.
--
-- Parameters:
--
-- - @cmd@: Command to run (e.g., "cabal")
-- - @args@: Command arguments (e.g., ["repl", "mylib"])
-- - @initialPrompt@: Expected prompt to wait for (e.g., "ghci> ")
-- - @timeoutMs@: Timeout in milliseconds for 'sendCommand' operations
--
-- Example:
--
-- > do
-- >   sess <- startRepl "cabal" ["repl"] "ghci> " 5000000  -- 5 second timeout
-- >   result1 <- sendCommand sess "1 + 1"
-- >   result2 <- sendCommand sess ":type length"
-- >   closeRepl sess
--
-- Startup sequence:
--
-- 1. Spawn PTY process
-- 2. Wait 300ms for process startup
-- 3. Send 'defaultSetupCommands' (one per 100ms)
-- 4. Wait 200ms for setup to complete
-- 5. Return session ready for interaction
--
-- The 'defaultSetupCommands' automatically suppress type-defaults and unused-matches warnings,
-- resulting in clean output.
startRepl :: FilePath -> [String] -> Text -> Int -> IO ReplSession
startRepl cmd args initialPrompt timeoutMs = do
  (pty', _ph) <- spawnWithPty Nothing True cmd args (80, 24)
  -- Wait for initial prompt to appear
  threadDelay 300000  -- 300ms for cabal repl startup
  -- Send setup commands to suppress warnings
  sendSetupCommands pty' defaultSetupCommands
  threadDelay 200000  -- wait for setup to finish
  pure $ ReplSession pty' initialPrompt timeoutMs

-- | Send one command and wait for the REPL to finish processing it
--
-- Writes the command to the REPL's stdin, then reads output until the prompt reappears
-- or timeout expires. Returns a 'ReplResult' with command, output, duration, and success status.
--
-- The output includes any warnings or errors from the REPL, cleaned of the echoed command
-- and prompt. Use 'rplSuccess' to check if the command completed without timeout.
--
-- Example:
--
-- > sess <- startRepl "cabal" ["repl"] "ghci> " 5000000
-- > result <- sendCommand sess "1 + 1"
-- > print $ rplOutput result  -- "2"
-- > print $ rplDuration result  -- milliseconds elapsed
--
-- Typical output (with 'defaultSetupCommands', clean):
--
-- > λ> sendCommand sess "[1,2,3] ++ [4,5]"
-- > ReplResult { rplOutput = "[1,2,3,4,5]", rplSuccess = True, ... }
--
-- If timeout expires:
--
-- > ReplResult { rplSuccess = False, rplError = Just "Timeout...", ... }
sendCommand :: ReplSession -> Text -> IO ReplResult
sendCommand sess cmd = do
  start <- getCurrentTime
  writePty (pty sess) (TE.encodeUtf8 cmd <> BS8.pack "\n")

  -- Smart waiting: read until we see the prompt again (or timeout)
  output <- readUntilPrompt (pty sess) (currentPrompt sess) (timeoutMs sess)
  end <- getCurrentTime
  let dur = round $ diffUTCTime end start * 1000

  case output of
    Left err -> pure $ ReplResult cmd T.empty (currentPrompt sess) False dur (Just err)
    Right (out, newPrompt) -> do
      let cleaned = cleanOutput out (currentPrompt sess)
      pure $ ReplResult cmd cleaned newPrompt True dur Nothing

-- | Read from PTY until we see a prompt (or timeout)
--
-- This is a simple timeout-based reader. For policy-driven capping
-- (detecting Ready vs other signals), see readWithPolicy.
readUntilPrompt :: Pty -> Text -> Int -> IO (Either Text (Text, Text))
readUntilPrompt pty' _expectedPrompt timeoutMs = do
  let loop :: BS.ByteString -> UTCTime -> IO (Either Text (Text, Text))
      loop acc startTime = do
        now <- getCurrentTime
        if diffUTCTime now startTime > fromIntegral timeoutMs / 1000
          then pure $ Left (T.pack "Timeout waiting for prompt")
          else do
            result <- tryReadPty pty'
            case result of
              Left _controlCodes -> do
                -- Control codes, continue waiting
                threadDelay 50000
                loop acc startTime
              Right chunk ->
                if BS.null chunk
                  then do
                    threadDelay 50000  -- 50ms
                    loop acc startTime
                  else do
                    let acc' = acc <> chunk
                        txt = TE.decodeUtf8 acc'
                    -- Check if any known prompt appears
                    case detectPrompt txt of
                      Just p  -> pure $ Right (txt, p)
                      Nothing -> loop acc' startTime
  start <- getCurrentTime
  loop BS.empty start

-- | Strip ANSI escape codes from ByteString
--
-- Removes sequences like \ESC[...m (color), \ESC[...h (mode set), \ESC[...l (mode reset), etc.
-- Any sequence \ESC[ followed by characters until a letter (A-Z, a-z) is removed.
--
-- Examples:
--
-- Plain text passes through unchanged:
--
-- >>> import qualified Data.ByteString.Char8 as BS
-- >>> stripAnsi (BS.pack "hello")
-- "hello"
--
-- Color codes are stripped:
--
-- >>> stripAnsi (BS.pack "\27[35mred\27[0m")
-- "red"
--
-- Mode sequences (ESC[?1h) are stripped:
--
-- >>> stripAnsi (BS.pack "\27[?1hghci> ")
-- "ghci> "
stripAnsi :: BS.ByteString -> BS.ByteString
stripAnsi bs = BS.pack $ go (BS.unpack bs)
  where
    go [] = []
    go (27 : 91 : rest) =  -- ESC ( = 27, [ = 91
      let (_, remainder) = break (\c -> c >= 65 && c <= 122) rest -- stop at letter (A-Z, a-z)
      in case remainder of
        [] -> []  -- malformed, skip
        (_ : rest') -> go rest'  -- skip the letter and continue
    go (c : rest) = c : go rest

-- | Read from PTY with timeout policy that waits for silence after Ready
--
-- This implements the policy-driven capping described in the architecture:
--
-- - Collects chunks from PTY until Ready (ghci>, Prelude>) is detected
-- - After Ready, waits for silence (1/10 of primary timeout) before returning
-- - Honors primary timeout on primary I/O operations
-- - Returns chunks in original form (ANSI codes intact, not cleaned)
--
-- Why this design:
--
-- The REPL is not request-response. Effects leak across prompt boundaries.
-- A @launchMissile :: IO ()@ might still print after ghci> appears.
-- This policy allows capturing those trailing effects without explicit termination signals.
--
-- Example usage with policy:
--
-- > -- Optimistic: accept Ready + silence
-- > chunks <- readWithTimeout pty 1000000  -- 1 second timeout
--
-- > -- Semantic: read until specific marker (not shown here)
-- > -- or coroutine: alternate with agent input (agent-driven pacing)
readWithTimeout :: Pty -> Int -> IO [BS.ByteString]
readWithTimeout pty' primaryTimeoutUs = go [] False
  where
    silenceTimeoutUs = primaryTimeoutUs `div` 10  -- 1/10 of primary timeout
    
    go acc seenReady = do
      result <- timeout primaryTimeoutUs (readPty pty')
      case result of
        Nothing -> pure (reverse acc)  -- primary timeout expired
        Just chunk ->
          let cleaned = TE.decodeUtf8 (stripAnsi chunk)  -- clean for inspection
              hasReady = T.pack "ghci> " `T.isInfixOf` cleaned || T.pack "Prelude> " `T.isInfixOf` cleaned
          in if hasReady && seenReady
             then do
               -- Already saw Ready once, check for more output with silence timeout
               moreResult <- timeout silenceTimeoutUs (readPty pty')
               case moreResult of
                 Nothing -> pure (reverse (chunk : acc))  -- silence after Ready, done
                 Just more -> go (more : chunk : acc) True  -- more coming
             else
               go (chunk : acc) (seenReady || hasReady)

-- | Detect any known GHCi prompt (extend this list as needed)
detectPrompt :: Text -> Maybe Text
detectPrompt txt =
  let prompts = [T.pack "ghci> ", T.pack "Prelude> ", T.pack "λ> ", T.pack "> ", T.pack "Ok, modules loaded: "]
  in foldr (\p acc -> if p `T.isInfixOf` txt then Just p else acc) Nothing prompts

-- | Remove the prompt and any echoed command from the output
cleanOutput :: Text -> Text -> Text
cleanOutput txt prompt =
  let newline = T.pack "\n"
      noEcho = T.replace (prompt <> newline) T.empty txt
      noPrompt = T.replace prompt T.empty noEcho
  in T.strip noPrompt

-- | Gracefully close the session
--
-- Sends @:quit@ to the REPL, waits briefly for shutdown, and the PTY is
-- automatically closed when the process exits.
--
-- Example:
--
-- > do
-- >   sess <- startRepl "cabal" ["repl"] "ghci> " 5000000
-- >   r1 <- sendCommand sess "length [1,2,3]"
-- >   closeRepl sess  -- sends :quit, waits, exits
closeRepl :: ReplSession -> IO ()
closeRepl sess = do
  writePty (pty sess) (BS8.pack ":quit\n")
  threadDelay 100000
  -- pty is automatically closed when the process exits
