# grepl

[![Hackage](https://img.shields.io/hackage/v/grepl.svg)](https://hackage.haskell.org/package/grepl)
[![Build Status](https://github.com/tonyday567/grepl/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/grepl/actions?query=workflow%3Ahaskell-ci)

## Overview

`grepl` is evolving from a file-based message passing protocol into a **circuit-based REPL framework** using the `circuits` library. This enables **first-principles type-safe REPL construction** where I/O semantics, resource management, and duality are made explicit through the free traced monoidal category.

### Current Architecture (Legacy)

The original implementation uses **named pipes** (FIFOs) to decouple input/output streams from console applications, enabling agents to multiplex queries across GHCi sessions without blocking on console behavior.

### New Architecture (In Development)

We are building a new REPL using `Circuit (Kleisli IO) t a b` where:
- **Input side**: `Circuit (Kleisli IO) Either String ()` — user input → REPL actions
- **Output side**: `Circuit (Kleisli IO) Either () String` — REPL operations → user output  
- **Resource management**: `Loop` constructor handles safe acquire/use/release of PTY and process handles

This approach makes I/O duality, resource management, and compositional semantics explicit.

## Architecture

### Core Components (Legacy Named-Pipe Protocol)

- **ChannelConfig** — Configuration for `cabal repl` process execution, specifying command, project directory, and named pipe paths.
- **channel** — Spawns a `cabal repl` process with stdin/stdout/stderr wired to named pipes, returning a process handle.
- **Watcher** — Monitors markdown log files for changes, enabling agents to react to query results in real-time.
- **Named Pipe Pattern** — Decouples process I/O, enabling reliable agent interaction with console applications.

### Design Rationale (Legacy)

Named pipes provide a stable interface for agent workflows:
- Agents write queries to stdin FIFO without blocking on console buffering.
- Stdout and stderr are logged to markdown files, preserving interaction history for agent analysis.
- Process lifecycle is independent of I/O, allowing agents to multiplex queries across sessions.
- Watcher observes markdown log files, triggering agent reactions to completed queries.

This pattern is proven robust for interactive code exploration in agentic contexts.

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

### Resource Management via Loop

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

### First-Principles Development: Breaking Down "REPL"

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

### Development Flow

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

## Integration with Agentic Systems

`grepl` is designed for systems where:
- Agents coordinate multiple tool interactions
- Interaction history must be preserved for auditing and learning
- Queries are dynamic and driven by prior results
- Process reliability matters more than console ergonomics

The named pipe design makes it ideal for:
- **Type exploration** — Query GHCi types, parse results, refine queries
- **Code generation** — Generate code snippets, test them, iterate
- **Session multiplexing** — Run multiple GHCi instances for parallel exploration
- **Markdown-driven workflows** — Watch log files to trigger agent decisions

## Design Highlights

- **File-based I/O** — No complex serialization; lines in, lines out
- **Markdown logging** — Human-readable history suitable for agent learning
- **Async watchers** — Background file monitoring without polling overhead
- **Process independence** — Lifecycle decoupled from I/O for reliability

## Next Steps

⟝ **Phase 1: Foundations** (current)
- ✅ Integrate `circuits` library as local dependency
- ✅ Create `Grepl.Repl` module with circuits access
- ⟝ Study circuit patterns in examples (resource-file.md, loop-examples.md)
- ⟝ Design individual circuits for Read, Evaluate, Prompt, Loop operations

⟝ **Phase 2: Implementation** (planned)
- Build `readCircuit :: Circuit (Kleisli IO) Either () String` (read from REPL)
- Build `writeCircuit :: Circuit (Kleisli IO) Either String ()` (write to REPL)
- Build `promptCircuit` for prompt detection and state
- Compose into a full REPL loop via `Loop` with resource management

⟝ **Phase 3: Integration**
- Test dual semantics: `lower readCircuit` and `lower writeCircuit` both work correctly
- Verify resource safety: resources are always cleaned up
- Add prompt variations (ghci>, λ>, etc.)
- Integrate with existing grepl architecture for backward compatibility

⟝ **Phase 4: Agent Integration**
- Connect circuit-based REPL to agentic workflows
- Support multiplexing across multiple REPL sessions
- Enable bidirectional control (agent ↔ REPL)

## Documentation

See [Grepl](https://hackage.haskell.org/package/grepl/docs/Grepl.html) and [Grepl.Watcher](https://hackage.haskell.org/package/grepl/docs/Grepl-Watcher.html) for detailed API documentation.

Circuits documentation: See `~/circuits/` for core library and examples.

## Related Work

- **circuits** — Free traced monoidal category, providing the foundation for circuit-based REPL
- **agent-fork** — Similar harness for pi executable, extending the named-pipe pattern
- **cabal** — The underlying project/REPL tool
- **GHCi** — The Haskell interactive environment
