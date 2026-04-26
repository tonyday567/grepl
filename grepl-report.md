# grepl Library Report

**Purpose:** File-based message passing protocol for querying GHCi instances. Agentic type wrangling and interactive code exploration.

**Repository:** https://github.com/tonyday567/grepl  
**Version:** 0.1.0.0  
**License:** BSD-3-Clause  

---

## README Status

❌ **No readme.md present**

This is a gap. The cabal synopsis/description exist but no narrative documentation. For an agentic library, this should be documented (see agent-fork pattern).

---

## Dependencies & Structure

**Library:**
- `Grepl` — ChannelConfig, process management with named pipes
- `Grepl.Watcher` — FSNotify for markdown file watching

**Executable:**
- `grepl-explore` (app/grepl-explore.hs) — CLI driver

**Dependency Analysis:**
```
base >=4.14 && <5          (standard)
process >=1.6 && <1.7      (spawning cabal repl)
async >=2.2 && <2.3        (concurrent tasks)
directory >=1.3 && <1.4    (file checks)
fsnotify >=0.4 && <0.5     (file watching)
stm >=2.5 && <2.6          (TChan message passing)
filepath >=1.4 && <1.5     (path manipulation)

(exe only)
time >=1.9 && <1.10        (timing utilities)
optparse-applicative >=0.17 && <0.20  (CLI parsing)
perf (local, sibling library)
```

**Pattern notes:**
- Minimal, focused deps—no heavy frameworks
- Concurrency via async + STM (clean, functional)
- File I/O for logs (markdown output)
- Local dependency on `perf` suggests tight integration with measurement library

---

## Code Quality Signals

**Extensions:**
- GHC2024 (modern)
- No extra pragmas observed (good sign—GHC2024 covers what's needed)

**Haddock Coverage:**
- `Grepl` — 80% (4/5 items documented, missing: module header)
- `Grepl.Watcher` — 66% (2/3 items documented, missing: module header)
- **Overall:** Module headers are missing but individual functions have doc comments

**Code Patterns:**
- Named pipe (FIFO) pattern for decoupled I/O ✓
- Explicit type annotations throughout ✓
- Clear naming (ensureFifo, channel, handleEvent) ✓
- Helper functions well-commented ✓

**GHC Options:**
```
-Wall -Wcompat -Widentities -Wincomplete-record-updates 
-Wincomplete-uni-patterns -Wpartial-fields -Wredundant-constraints
```
Solid, comprehensive warning set.

---

## Issues to Address in Checklist

| Issue | Severity | Action |
|-------|----------|--------|
| Author: Anonymous → Tony Day | High | Update cabal file |
| No readme.md | High | Create with agentic focus |
| Missing module headers in Grepl and Grepl.Watcher | Medium | Add haddock module documentation |
| tested-with: only GHC 9.14.1 | Medium | Expand to last 3 versions (9.14, 9.12, 9.10) |
| No CHANGELOG.md content | Low | Populate with release notes |
| No cabal.project | Low | Create default pattern (optional for single-package libs) |

---

## Design Observations

**What grepl Does Well:**
1. **Named Pipe Architecture** — Proven robust for handling cabal repl's buffering issues
2. **Stateless I/O** — Files and FIFOs are the contract; process lifecycle is independent
3. **Agentic-First Design** — Markdown logging makes history available for agent analysis
4. **Concurrent Watcher** — FSNotify polling in background async thread

**Design Philosophy:**
- Decoupling: separate stdin/stdout/stderr into named pipes + log files
- History: all I/O logged to markdown for auditability
- Concurrency: async tasks for polling, STM for message passing
- Simplicity: no complex state machines, just I/O redirection

**Module Organization:**
- `Grepl` — Core process management (ChannelConfig, piping logic)
- `Grepl.Watcher` — Orthogonal concern (file watching for agent workflows)
- Separation of concerns is clean

---

## Readiness for Checklist

**Ready:** Most sections
- Code quality checks (ormolu, hlint, cabal-gild) will likely pass
- Dependency bounds are reasonable
- Build should be clean

**Needs Work:**
- Documentation (readme, haddock headers)
- Metadata (author, changelog, tested-with)
- Version bounds might need review (especially perf local dep)

**Estimated Effort:** 1-2 hours to standardize (mostly docs + metadata)

---

## Comparison to agent-fork

| Aspect | grepl | agent-fork |
|--------|-------|-----------|
| Readme | ❌ None | ✓ Complete |
| Module headers | ❌ Missing | ✓ Complete |
| Haddock coverage | 66-80% | 100% |
| tested-with | 9.14 only | 9.14, 9.12, 9.10 |
| Author | Anonymous | Tony Day |
| Maturity | Earlier (first pass) | Refined (following checklist) |

grepl is a good candidate for standardization via the checklist—it has solid code but needs documentation and metadata polish.
