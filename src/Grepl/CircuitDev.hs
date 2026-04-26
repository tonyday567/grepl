-- |
-- Module      : Grepl.CircuitDev
-- Copyright   : (c) 2026 Tony Day
-- License     : BSD-3-Clause
-- Maintainer  : tonyday567@gmail.com
--
-- Interactive development harness for circuit-based REPL.
--
-- This module provides helpers and re-exports for live `cabal repl` development.
-- Load this into GHCi and build circuits incrementally.
--
-- = Quick Start
--
-- @
-- $ cabal repl grepl
-- ghci> import Grepl.CircuitDev
-- ghci> :type Circuit
-- ghci> let c = lift' (+1) :: Circuit (->) (,) Int Int
-- ghci> testPure c 5
-- @
--
-- = With PTYs
--
-- @
-- ghci> (pty, ph) <- spawnCabalRepl
-- ghci> writePty pty (BS.pack "1 + 1\\n")
-- ghci> readPty pty >>= BS.putStrLn
-- ghci> closePty pty
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
module Grepl.CircuitDev
  ( -- * Core circuit types and functions (re-exported from modules)
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

import Circuit
import Circuit.Circuit hiding (lower)
import Circuit.Traced
import Control.Arrow (Kleisli (..), runKleisli)
import Control.Category (Category (..))
import qualified Circuit.Circuit as CC
import qualified Data.ByteString.Char8 as BS
import System.Posix.Pty
import System.Process (ProcessHandle)
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
