# grepl

[![Hackage](https://img.shields.io/hackage/v/grepl.svg)](https://hackage.haskell.org/package/grepl)
[![Build Status](https://github.com/tonyday567/grepl/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/grepl/actions?query=workflow%3Ahaskell-ci)

## design and development

`grepl` is a general repl, a read-eval-print loop for use with general processes.

Grepl.Fifo uses the system concept of named pipes or FIFOs attached to a Process (using the process library).

Grepl.Cabal applies the generalised repl model on `cabal repl`

### FIFO

The Grepl.Fifo implementation uses **named pipes** (FIFOs) to decouple input/output streams from console applications, enabling agents to multiplex queries across GHCi sessions without blocking on console behavior.



Core Components

- **ChannelConfig** — Configuration for `cabal repl` process execution, specifying command, project directory, and named pipe paths.
- **channel** — Spawns a `cabal repl` process with stdin/stdout/stderr wired to named pipes, returning a process handle.
- **Watcher** — Monitors markdown log files for changes, enabling agents to react to query results in real-time.
- **Named Pipe Pattern** — Decouples process I/O, enabling reliable agent interaction with console applications.

Named pipes provide a stable interface for agent workflows:
- Agents write queries to stdin FIFO without blocking on console buffering.
- Stdout and stderr are logged to markdown files, preserving interaction history for agent analysis.
- Process lifecycle is independent of I/O, allowing agents to multiplex queries across sessions.
- Watcher observes markdown log files, triggering agent reactions to completed queries.

### Pty [other/pty.md]

We are building a new repl using:

- `Circuit (Kleisli IO) t a b` where:
- The idea in compact closed categories and the use of dual actions to code the repl.
- **Input side**: `Circuit (Kleisli IO) Either String ()` — user input → REPL actions
- **Output side**: `Circuit (Kleisli IO) Either () String` — REPL operations → user output
- **Resource management**: `Loop` constructor handles safe acquire/use/release of PTY and process handles

This approach makes I/O duality, resource management, and compositional semantics explicit.

