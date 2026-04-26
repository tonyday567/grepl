src/Repl.hs:9:1: warning: [GHC-66111] [-Wunused-imports]
    The import of ‘Control.Concurrent’ is redundant
      except perhaps to import instances from ‘Control.Concurrent’
    To import instances alone, use: import Control.Concurrent()
  |
9 | import Control.Concurrent (threadDelay)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

src/Repl.hs:10:1: warning: [GHC-66111] [-Wunused-imports]
    The import of ‘Control.Concurrent.Async’ is redundant
      except perhaps to import instances from ‘Control.Concurrent.Async’
    To import instances alone, use: import Control.Concurrent.Async()
   |
10 | import Control.Concurrent.Async (race, async)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

src/Repl.hs:11:1: warning: [GHC-66111] [-Wunused-imports]
    The import of ‘Data.List’ is redundant
      except perhaps to import instances from ‘Data.List’
    To import instances alone, use: import Data.List()
   |
11 | import Data.List (isInfixOf)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

<interactive>:1:1: warning: [GHC-18042] [-Wtype-defaults]
    • Defaulting the type variable ‘a0’ to type ‘Integer’ in the following constraints
        (Show a0) arising from a use of ‘print’ at <interactive>:1:1-5
        (Num a0) arising from a use of ‘it’ at <interactive>:1:1-5
    • In a stmt of an interactive GHCi command: print it

src/Repl.hs:9:1: warning: [GHC-66111] [-Wunused-imports]
    The import of ‘Control.Concurrent’ is redundant
      except perhaps to import instances from ‘Control.Concurrent’
    To import instances alone, use: import Control.Concurrent()
  |
9 | import Control.Concurrent (threadDelay)
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

src/Repl.hs:10:1: warning: [GHC-66111] [-Wunused-imports]
    The import of ‘Control.Concurrent.Async’ is redundant
      except perhaps to import instances from ‘Control.Concurrent.Async’
    To import instances alone, use: import Control.Concurrent.Async()
   |
10 | import Control.Concurrent.Async (race, async)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

src/Repl.hs:11:1: warning: [GHC-66111] [-Wunused-imports]
    The import of ‘Data.List’ is redundant
      except perhaps to import instances from ‘Data.List’
    To import instances alone, use: import Data.List()
   |
11 | import Data.List (isInfixOf)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

src/Grepl/Merger.hs:31:46: warning: [GHC-38856] [-Wunused-imports]
    The import of ‘void’ from module ‘Control.Monad’ is redundant
   |
31 | import Control.Monad (forever, when, unless, void)
   |                                              ^^^^

src/Grepl/Merger.hs:39:1: warning: [GHC-40910] [-Wunused-top-binds]
    Defined but not used: ‘ensureFifo’
   |
39 | ensureFifo path = do
   | ^^^^^^^^^^

src/Grepl.hs:51:1: warning: [GHC-66111] [-Wunused-imports]
    The import of ‘Control.Concurrent’ is redundant
      except perhaps to import instances from ‘Control.Concurrent’
    To import instances alone, use: import Control.Concurrent()
   |
51 | import Control.Concurrent (threadDelay)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

src/Grepl.hs:52:1: warning: [GHC-66111] [-Wunused-imports]
    The import of ‘Control.Concurrent.Async’ is redundant
      except perhaps to import instances from ‘Control.Concurrent.Async’
    To import instances alone, use: import Control.Concurrent.Async()
   |
52 | import Control.Concurrent.Async (async, race)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

src/Grepl.hs:54:1: warning: [GHC-66111] [-Wunused-imports]
    The import of ‘Data.List’ is redundant
      except perhaps to import instances from ‘Data.List’
    To import instances alone, use: import Data.List()
   |
54 | import Data.List (isInfixOf)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

