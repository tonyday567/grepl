# Development Tasks

## Immediate Priorities (From Live PTY Session)

Working session shows PTY is functional. Three concrete tasks to unlock `readUntilPrompt`:

### 1. ANSI Strip Function

**Problem:** Output is full of `\ESC[...m` escape codes.
```
\ESC[?1h\ESC=\ESC[;1m\ESC[35mwarning\ESC[0m
```

**Task:** Build `stripAnsi :: ByteString -> ByteString`
- Regex or manual parser for `\ESC[...m` sequences
- Preserve the actual content: `ghci> `, `2`, etc.
- Test on real output chunks

**Where:** `Grepl.Pty` module, export it for reuse

### 2. readWithTimeout Implementation

**Problem:** `readPty` returns arbitrary chunks. The result `2` arrived split across 4 chunks. 
Moreover, `ghci> ` is just `Ready` — effects can continue after the prompt.

**Task:** Implement timeout-based accumulation that detects `Ready` but doesn't assume termination
```haskell
readWithTimeout :: Pty -> Int -> IO [ByteString]
readWithTimeout pty timeoutUs = go [] False
  where
    go acc seenReady = do
      result <- timeout timeoutUs (readPty pty)
      case result of
        Nothing -> pure (reverse acc)  -- timeout, return accumulated
        Just chunk ->
          let cleaned = stripAnsi chunk
              readyNow = "ghci> " `BS.isInfixOf` cleaned
          in if readyNow && seenReady
             then timeout (timeoutUs `div` 10) (readPty pty) >>= \case
               Nothing -> pure (reverse (chunk : acc))  -- silence after Ready
               Just more -> go (more : chunk : acc) True  -- more coming
             else go (chunk : acc) (seenReady || readyNow)
```

**Test:** Send `"1 + 1\n"`, collect until Ready + silence, verify final result contains `"2"`.

### 3. Initial Setup Commands

**Problem:** Warnings (like `-Wtype-defaults`) clutter output and should be filtered at spawn time.

**Task:** After `spawnCabalRepl`, immediately send setup commands:
```haskell
let setupCommands = [":set -Wno-type-defaults", ...]
```

**Where:** In `Grepl.Pty` `spawnWithPty` or as a separate "initialize" step.

---

## Architecture Notes

### Streaming is Open-Ended

The REPL is not request-response. `ghci> ` is a **Ready** tag, not a Done signal. 
Effects leak across prompt boundaries. The cap is a **policy decision**, not structural.

### TChan for Decoupling

The PTY reader runs forever on a background thread, tagging chunks as they arrive. 
The consumer reads from `TChan` with a timeout policy. This decouples producer 
(streaming input) from consumer (policy-driven collection).

### Policy-Driven Capping

When to stop reading?
- Timeout silence after Ready
- Semantic signal (computation complete)
- Next User prompt appears
- Heuristic: effects are fast, cap immediately after Ready

The orchestrator picks the policy. The stream architecture supports all of them.

---

## Analysis

---

## Circuit-Based REPL Architecture (Planned)

### Core Insight: Dual I/O via Circuits

The fundamental abstraction is the **dual nature** of REPL interaction:

```haskell
-- User output: what the REPL produces
type ReplOutput = Circuit (Kleisli IO) Either () String
-- When lowered: () -> IO String

-- User input: what the user feeds the REPL
type ReplInput = Circuit (Kleisli IO) Either String ()
-- When lowered: String -> IO ()
```

When these circuits are `lower`ed:
- `ReplOutput` becomes `() -> IO String`: request output, get result from REPL
- `ReplInput` becomes `String -> IO ()`: send input, produce effect in REPL

They are **dual** in the category-theoretic sense: reversing types and effects.

⟝ Resource Management via Loop

The `Loop` constructor handles safe resource acquisition and release:

```haskell
-- The resource (PTY and ProcessHandle) lives in the feedback channel
replResource :: Circuit (Kleisli IO) Either () (Pty, ProcessHandle)
replResource = Circuit.Loop $ \case
  Right () -> do      -- Exit: release resources
    closePty pty
    terminateProcess ph
    pure (Right "Done")
  Left (pty, ph) -> do  -- Loop body: interact with REPL
    -- Send command, get output, decide to continue
    pure (Left (pty, ph))  -- or Right () to exit
```

This pattern mirrors the `resource-file.md` example: resources are acquired on entry, used in the loop body, and released when we `Right` out of the loop.

⟝ First-Principles Development: Breaking Down REPL

We develop the REPL incrementally, letter by letter, working out what each component entails on **both sides of the dual**:

#### **R: Read**

**Output side** (`() -> IO String`):
- Read a command/prompt from the REPL's stdout
- Return the bytes/string to the user
- Handle parsing and buffering

**Input side** (`String -> IO ()`):
- Receive a raw command string from user
- Pass it to the REPL's stdin
- Trigger the REPL to produce output

**Duality**: Reading (output) and writing (input) are inverses — one extracts, one injects.

#### **E: Evaluate**

**Output side**:
- After reading, the REPL evaluates the command
- Wait for evaluation to complete
- Return results (stdout) or errors (stderr)

**Input side**:
- Trigger evaluation by feeding the command
- Manage timeouts (the REPL may hang)
- Signal completion or failure

**Duality**: Requesting evaluation (output) vs. performing it (input).

#### **P: Prompt**

**Output side**:
- Detect the next REPL prompt (e.g., `ghci> `)
- This marks the end of one evaluation cycle
- Return prompt state for the next read

**Input side**:
- Maintain prompt expectation (what should we wait for?)
- Update prompt state as we interact
- Feed the prompt expectations to the REPL for proper flow

**Duality**: Observing prompts (output) vs. maintaining state for them (input).

#### **L: Loop**

**Output side**:
- The feedback channel carries `(Pty, ProcessHandle, PromptState)`
- Each iteration reads command, evaluates, returns prompt
- Exit when user signals quit

**Input side**:
- The feedback channel carries the same resource state
- Each iteration accepts a command string, sends to REPL, returns IO ()
- Exit when no more commands

**Duality**: The loop is the same loop from both perspectives — it closes the feedback channel in both directions.

🟣 Development Flow

1. **Start simple**: Build individual circuits for each operation (read, write, prompt detection)
2. **Compose**: Chain them using `Compose` to build larger operations
3. **Add state**: Use `Loop` with appropriate tensor to thread state (PTY, handle, prompt expectations)
4. **Test each dual**: Verify that `lower` on both input and output sides works correctly
5. **Integrate resources**: Fold in safe resource management via `Loop` exit paths
6. **Verify safety**: Ensure PTY and process handles are always cleaned up

### Tensor Choice: Either for Sequential State

We use the `Either` tensor for sequential feedback (as shown in `loop-examples.md`):

```haskell
-- Entry: Right ()   → initialize resources
-- Loop:  Left (pty, ph, state) → interact
-- Exit:  Right result → cleanup and exit
```

This gives us explicit control over the acquire-use-release cycle, making resource safety a **structural property** of the circuit.

### Examples & Patterns

See `~/circuits/examples/` for reference patterns:
- **loop-examples.md** — Loop patterns with `(,)` and `Either` tensors
- **resource-file.md** — Safe resource handling via Loop exit
- **circuit-dual.md** — Forward and backward directions in circuits
- **circuit-agent.md** — Agent-based circuit composition

## Usage

### Basic Setup

```haskell
import Grepl

-- Spawn a cabal repl session with default configuration
let cfg = defaultChannelConfig
ph <- channel cfg
```

### Custom Configuration

```haskell
let cfg = ChannelConfig
      { processCommand = "cabal repl"
      , projectDir = "./my-project"
      , stdinPath = "/tmp/ghci-in"
      , stdoutPath = "./log/cabal-repl-stdout.md"
      , stderrPath = "./log/cabal-repl-stderr.md"
      }
ph <- channel cfg
```

### Executable Channel

For executable targets, use a separate configuration:

```haskell
let cfg = exeChannelConfig  -- defaults to "cabal repl grepl-explore"
ph <- channel cfg
```

### Agent Workflows

```haskell
-- Write a type query to the stdin FIFO (non-blocking)
writeFile "/tmp/ghci-in" ":type someFunction\n"

-- Read logged output asynchronously
stdout <- readFile "./log/cabal-repl-stdout.md"
stderr <- readFile "./log/cabal-repl-stderr.md"

-- Watch for file changes and react
chan <- watchMarkdown "./log"
-- handle file events from chan...

-- Analyze results, branch on outcome, re-query as needed
```

