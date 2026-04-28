# Development Tasks

## Immediate Priorities (From Live PTY Session)

Working session shows PTY is functional. Three concrete tasks to unlock `readUntilPrompt`:

### 1. ANSI Strip Function

✅ **DONE**

**Implementation:** Manual parser (no regex dependency)
- Recognizes `\ESC[` (27, 91) followed by any sequence until A-Z or a-z letter
- Strips the entire escape sequence, preserves content
- Tested: `\ESC[35mred\ESC[0m` → `"red"`
- Exported from `Grepl.Pty`, available in live session

**Function:** `stripAnsi :: ByteString -> ByteString`

### 2. readWithTimeout Implementation

✅ **DONE**

**Implementation:** Policy-driven timeout with silence detection
- Accumulates chunks from PTY using primary timeout
- Detects Ready (ghci>, Prelude>) by cleaning ANSI codes first
- After first Ready, uses 1/10 timeout to catch trailing effects
- Returns chunks when silence detected after Ready
- Handles primary timeout expiry (returns accumulated chunks)

**Function:** `readWithTimeout :: Pty -> Int -> IO [ByteString]`
- Parameter: microseconds for primary timeout
- Returns: list of ByteString chunks (not stripped, original)

**Design principle:** The cap is a policy decision. Ready is not termination.

### 3. Initial Setup Commands

✅ **DONE**

**Implementation:** Automatic setup on `startRepl`
- `defaultSetupCommands`: list of `:set` directives (suppressions + prompt config)
  - `:set -Wno-type-defaults`
  - `:set -Wno-unused-matches`
  - `:set prompt "ghci> "` (enforce consistent prompt)
- `sendSetupCommands pty cmds`: sends commands with 100ms spacing
- `startRepl`: automatically sends `defaultSetupCommands` after spawn

**Result:** cabal repl starts clean without warning noise.

**Functions:**
- `defaultSetupCommands :: [Text]` 
- `sendSetupCommands :: Pty -> [Text] -> IO ()`
- `startRepl` now includes automatic setup

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

---

## Completed Foundation Tasks

✅ **stripAnsi** — removes ANSI escape codes from PTY output
- Handles `\ESC[...m` sequences (color, style codes)
- Preserves actual content
- Used for prompt detection and logging

✅ **readWithTimeout** — policy-driven PTY reading
- Accumulates chunks until Ready (ghci>)
- Waits for silence (1/10 timeout) after Ready
- Captures trailing effects that leak past the prompt
- Honors primary timeout on primary I/O

✅ **defaultSetupCommands** — quiet REPL startup
- `:set -Wno-type-defaults` — suppress inference noise
- `:set -Wno-unused-matches` — suppress pattern warnings
- `:set prompt "ghci> "` — enforce consistent prompt
- Automatically sent by `startRepl`

### Testing the Foundation

```haskell
-- In ghci:
let sess <- startRepl "cabal" ["repl"] "ghci> " 5000000
result <- sendCommand sess "1 + 1"
closeRepl sess
```

The PTY interface is now clean and ready for next-layer work:
- Build `Tagged` stream consumer from raw ByteString chunks
- Implement TChan-based producer/consumer split
- Layer Circuit on top of the streaming interface

### Next Work

1. **Tagged stream consumer** — parse chunks into `(Tag, Text)` events
2. **TChan watcher** — background PTY reader pushing to channel
3. **Circuit layer** — bidirectional async channel as Circuit term
4. **Agent integration** — turn `Circuit (Kleisli IO) t Tagged Tagged` into an operational Agent

