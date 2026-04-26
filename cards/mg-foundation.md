# MG Foundation: Multi-Agent REPL Coordination

**Intent:** Build a minimal, observable, shareable development environment where agents coordinate via grepl's file-based protocol.

**Scope:**

⟜ Use grepl (cabal repl via named pipes, stdout logged to markdown)
⟜ Use agent-fork patterns (agent state in files, not process memory)
⟜ Write Haskell experiments directly in this card
⟜ All artifacts persist: cabal repl output, agent state, history

**Rapid Loop:**

Write code → execute → observe output in markdown log → decide next move

All state is visible. No hidden buffers. No GUI layer. Just text and files.

**Navigation:**

⟜ **card** ⟜ design and intent (this file)
⟜ **log/** ⟜ cabal repl stdout/stderr as experiment runs
⟜ **agents/** ⟜ agent state files (one per agent)
⟜ **examples/** ⟜ working examples once we verify them

---

## Experiment 1: Verify Grepl

**Goal:** Start a grepl session, write a query, read the result from the log.

**Verification:** "42" appears in the markdown log.

```haskell
-- Start grepl
-- Write "42" to stdin FIFO
-- Check log contains "42"
```

---

## Experiment 2: Two Agents, Shared Log

**Goal:** Two agents read from the same grepl log, each updates their own state.

**Verification:** Both agents see the same query result, update independently.

---

## Experiment 3: Turn-Based Coordination

**Goal:** Conductor writes query → agents read result → agents write to conductor's inbox.

**Verification:** Clean dialogue, no chaos.

---

Ready to run Experiment 1?
