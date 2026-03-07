# grepl

[![Hackage](https://img.shields.io/hackage/v/grepl.svg)](https://hackage.haskell.org/package/grepl)
[![Build Status](https://github.com/tonyday567/grepl/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/grepl/actions?query=workflow%3Ahaskell-ci)

## Overview

`grepl` is a file-based message passing protocol for querying GHCi instances. It enables reliable interaction with `cabal repl` in agentic workflows for type wrangling, code exploration, and interactive problem-solving.

The library uses **named pipes** (FIFOs) to decouple input/output streams from the notoriously buffering-prone console applications. This design pattern decouples process I/O, allowing agents to multiplex queries across GHCi sessions without blocking on console behavior.

## Architecture

### Core Components

- **ChannelConfig** — Configuration for `cabal repl` process execution, specifying command, project directory, and named pipe paths.
- **channel** — Spawns a `cabal repl` process with stdin/stdout/stderr wired to named pipes, returning a process handle.
- **Watcher** — Monitors markdown log files for changes, enabling agents to react to query results in real-time.
- **Named Pipe Pattern** — Decouples process I/O, enabling reliable agent interaction with console applications.

### Design Rationale

Named pipes provide a stable interface for agent workflows:
- Agents write queries to stdin FIFO without blocking on console buffering.
- Stdout and stderr are logged to markdown files, preserving interaction history for agent analysis.
- Process lifecycle is independent of I/O, allowing agents to multiplex queries across sessions.
- Watcher observes markdown log files, triggering agent reactions to completed queries.

This pattern is proven robust for interactive code exploration in agentic contexts.

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

## Documentation

See [Grepl](https://hackage.haskell.org/package/grepl/docs/Grepl.html) and [Grepl.Watcher](https://hackage.haskell.org/package/grepl/docs/Grepl-Watcher.html) for detailed API documentation.

## Related Work

- **agent-fork** — Similar harness for pi executable, extending the named-pipe pattern
- **cabal** — The underlying project/REPL tool
- **GHCi** — The Haskell interactive environment
