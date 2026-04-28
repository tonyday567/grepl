# Circuit REPL Development Guide

## Quick Start

Start a live GHCi session with all circuit modules loaded:

```bash
cd ~/haskell/grepl
cabal repl grepl
```

Then in GHCi:

```haskell
ghci> import Grepl.CircuitDev
ghci> :type Circuit
ghci> :type Kleisli
ghci> :type lower  -- Oops, this doesn't exist, use lower from CC module
```

## Building Simple Circuits

### 1. Lift a pure function

```haskell
ghci> let c = lift' (+1) :: Circuit (->) (,) Int Int
ghci> testPure c 5
6
```

Key points:
- `lift'` embeds a pure function `arr a b` into `Circuit arr t a b`
- Type annotation needed to specify the tensor (here `(,)` for simultaneous feedback)
- `testPure` runs it and prints the result

### 2. Compose circuits

```haskell
ghci> let c1 = lift' (+1) :: Circuit (->) (,) Int Int
ghci> let c2 = lift' (*2) :: Circuit (->) (,) Int Int
ghci> let composed = c2 `compose'` c1
ghci> :type composed
Circuit (->) (,) Int Int
```

Note: `compose'` is right-associative, so `f `compose'` g` means "first g, then f" (same as `f . g`).

### 3. Test with explicit tensor

When tensor is ambiguous, specify it:

```haskell
ghci> let c = lift' (+ 1) :: Circuit (->) Either Int Int
ghci> testPure c 5
6
```

Both `(,)` (simultaneous) and `Either` (sequential) work for simple lifts.

## Understanding Tensors

### (,) Tensor: Simultaneous Feedback

Lazy knot-tying. The feedback and output coexist in one step.

```haskell
-- Circuit.Loop handles the feedback internally
ghci> let loopTuple = Circuit.Loop $ \(state, input) -> (state + 10, state + input)
ghci> :type loopTuple
Circuit (->) (,) Int Int
```

### Either Tensor: Sequential Feedback

Explicit iteration with entry (`Right`) and loop (`Left`).

```haskell
ghci> let loopEither = Circuit.Loop $ \case
        Right n -> Left (0, n)           -- Entry: initialize
        Left (acc, n) -> 
          if n > 0
            then Left (acc + n, n - 1)   -- Loop: continue
            else Right acc               -- Exit: return result
```

## Next: Building IO Circuits

To work with IO (PTY, process I/O), we'll use:

```haskell
ghci> import Control.Arrow (Kleisli(..))
ghci> let ioCircuit = lift' print :: Circuit (Kleisli IO) (,) String ()
```

The `Kleisli IO` arrow wraps `a -> IO b` operations, allowing us to:
- Acquire resources (PTY, process handles)
- Send/receive from the REPL
- Clean up safely via `Loop` exit paths

## Key Patterns (from ~/circuits/examples/)

### Pattern 1: Resource Management via Loop

See `resource-file.md`:

```haskell
fileResource :: FilePath -> Circuit (Kleisli IO) Either () String
fileResource path = ...  -- in development
```

Resources live in the feedback channel. They're acquired on `Right ()` entry, used in `Left state` loop body, and released when exiting via `Right result`.

### Pattern 2: Composing Operations

Chains of `Compose` build complex circuits from simple components:

```haskell
readOp = lift' readLine :: Circuit (Kleisli IO) t String String
processOp = lift' parseCommand :: Circuit (Kleisli IO) t String Command
handleOp = lift' executeCommand :: Circuit (Kleisli IO) t Command String

-- Compose: read → parse → execute
fullPipeline = handleOp `compose'` processOp `compose'` readOp
```

### Pattern 3: Duality via Dual Constructor

For bidirectional operations (later):

```haskell
-- If we have a forward circuit
fwd :: Circuit (->) t a b

-- We can make it backward
bwd :: Circuit (->) t b a
```

(This requires the `Back` encoding, which is in the circuits examples.)

## Workflow for Building the REPL

1. **Start with read**: Build `readCircuit :: Circuit (Kleisli IO) Either () String`
   - Acquires PTY in `Right ()` 
   - Reads data in loop body `Left (pty, handle)`
   - Returns data back to caller

2. **Then write**: Build `writeCircuit :: Circuit (Kleisli IO) Either String ()`
   - Takes input string
   - Sends to PTY in loop body
   - Returns `()`

3. **Add prompt detection**: Stateful circuit that detects `ghci> `, `λ> `, etc.

4. **Compose them**: Chain operations into a full REPL loop

5. **Test duality**: Verify both directions work with `lower`

## Tips & Tricks

### View type information

```haskell
ghci> :type Circuit.Circuit.lower
ghci> :type reify
ghci> :type toHyper
```

### Test function signatures before circuits

```haskell
ghci> let f = \(state, input) -> (state + 10, state + input)
ghci> :type f
((,) Int Int) -> ((,) Int Int)

-- Now wrap it
ghci> let c = lift' f :: Circuit (->) (,) Int Int
```

### Step through compositions manually

```haskell
ghci> let c1 = lift' (+1) :: Circuit (->) (,) Int Int
ghci> let c2 = lift' (*2) :: Circuit (->) (,) Int Int
ghci> let c12 = c2 `compose'` c1

-- Test pieces
ghci> testPure c1 5   -- Should be 6
6
ghci> testPure c2 6   -- Should be 12
12
ghci> testPure c12 5  -- Should be 12 (5+1 then *2)
```

### Look at examples in ~/circuits/examples/

- `loop-examples.md` — Detailed Loop patterns
- `resource-file.md` — Safe resource handling
- `circuit-agent.md` — Agent-based composition
- `echo-server.md` — Simple I/O circuit

## Common Errors

### "Ambiguous type variable t"

```
• Ambiguous type variable 't0' arising from a use of 'testPure'
```

Fix: Specify the tensor explicitly in the type annotation.

```haskell
ghci> let c = lift' (+1) :: Circuit (->) (,) Int Int  -- (,) is explicit
```

### "No instance for Category arr"

Happens when using `lower` without proper constraints. Use the helper `testPure` instead, which handles constraints.

### Type mismatch in composition

```
let composed = f `compose'` g  -- g runs first, then f
```

Remember: composition is right-to-left like `(.)`, not left-to-right.

## Next Session

When you return, start with:

```bash
cabal repl grepl
ghci> import Grepl.CircuitDev
-- Pick up where you left off!
```

All modules are reloaded automatically, so your REPL session is a living scratch space.
