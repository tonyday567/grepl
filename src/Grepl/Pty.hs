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
-- ghci> sess <- startRepl "cabal" ["repl"] "ghci> " 5000
-- ghci> result <- sendCommand sess "1 + 1"
-- ghci> closeRepl sess
-- @
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

-- | Start a new stateful REPL session (e.g. @"cabal" ["repl", "mylib"]@)
startRepl :: FilePath -> [String] -> Text -> Int -> IO ReplSession
startRepl cmd args initialPrompt timeoutMs = do
  (pty', _ph) <- spawnWithPty Nothing True cmd args (80, 24)
  -- Wait for initial prompt to appear
  threadDelay 200000  -- 200ms
  pure $ ReplSession pty' initialPrompt timeoutMs

-- | Send one command and wait for the REPL to finish processing it
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

-- | Read from PTY until we see the expected prompt (or timeout)
readUntilPrompt :: Pty -> Text -> Int -> IO (Either Text (Text, Text))
readUntilPrompt pty' expectedPrompt timeoutMs = do
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
closeRepl :: ReplSession -> IO ()
closeRepl sess = do
  writePty (pty sess) (BS8.pack ":quit\n")
  threadDelay 100000
  -- pty is automatically closed when the process exits
