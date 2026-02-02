#!/bin/bash
# listener — spawn cabal repl, capture stdout/stderr forever

set -e

REPL_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG_DIR="$REPL_DIR/log"
FIFO_IN="/tmp/ghci-in"

mkdir -p "$LOG_DIR"

# Ensure FIFO exists
if [[ ! -p "$FIFO_IN" ]]; then
  echo "[!] FIFO not found at $FIFO_IN"
  echo "    Create it first: mkfifo $FIFO_IN"
  exit 1
fi

echo "[*] Starting listener in $REPL_DIR"
echo "[*] Reading from: $FIFO_IN"
echo "[*] Writing to: $LOG_DIR/cabal-repl-*.md"

export PATH="$HOME/.ghcup/bin:$PATH"

# Spawn cabal repl, redirect streams to log files
cd "$REPL_DIR"
cabal repl < "$FIFO_IN" >> "$LOG_DIR/cabal-repl-stdout.md" 2>> "$LOG_DIR/cabal-repl-stderr.md" &
CABAL_PID=$!

echo "[*] Cabal repl spawned (PID: $CABAL_PID)"
echo "[*] Listening... (ctrl+C to stop)"

# Wait for cabal process
wait $CABAL_PID
CABAL_EXIT=$?

echo "[!] Cabal process exited with code $CABAL_EXIT"
