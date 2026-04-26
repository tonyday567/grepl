-- |
-- Module      : Grepl.Repl
-- Copyright   : (c) 2026 Tony Day
-- License     : BSD-3-Clause
-- Maintainer  : tonyday567@gmail.com
--
-- REPL integration with circuits.
--
-- = Overview
--
-- @Grepl.Repl@ bridges grepl's file-based channel protocol with circuits-based
-- computation and agent workflows.
--
-- Imports and re-exports circuits modules for access to circuit abstractions:
-- - "Circuit" — Core circuit abstraction
-- - "Circuit.Circuit" — Circuit GADT and operations
-- - "Circuit.Traced" — Traced monoidal category
-- - "Circuit.Hyper" — Hyperfunctions (Church encoding)
--
-- = Usage
--
-- Import circuits modules within REPL context, using qualified imports
-- for items that appear in multiple modules (e.g., @lower@):
--
-- > import Grepl.Repl
-- > import qualified Circuit.Circuit as C
-- > import qualified Circuit.Hyper as H
-- >
-- > -- Use C.lower vs H.lower to disambiguate
--
-- = Design
--
-- This module serves as a consolidation point for circuits-based computation
-- in agentic REPL workflows. Re-exports provide agent access to all circuit
-- abstractions through a single import point.
module Grepl.Repl
  ( -- * Base circuit module (re-exported)
    module Circuit,
    
    -- * Circuit GADT (Circuit.Circuit re-exported, minus lower)
    module Circuit.Circuit,
    
    -- * Traced category (re-exported)
    module Circuit.Traced,
    
    -- * Hyperfunctions (Circuit.Hyper, qualified to avoid lower conflict)
    module Circuit.Hyper,
  )
where

import Circuit
import Circuit.Circuit hiding (lower)
import Circuit.Traced
import Circuit.Hyper
