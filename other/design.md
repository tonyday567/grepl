# Design Notes

## Message Bus Architecture: REPL as First-Class Participant



A REPL is largely a narrative overlaid on a bidirectional process that consists of:

⟜ a long-lived process, held in an environment.
⟜ connected by three conduits stdin, stdout & stderr
⟜ these conduits 

A generalized REPL consists of:

A long-lived backend process (or equivalent stateful computation) that lives in some managed environment.

This could be:
An external executable (via System.Process).
An embedded interpreter (e.g., a custom evaluator, GHC API, LuaJIT, etc.).
A stateful monad transformer stack that persists across interactions.
Even a remote service over a socket.

Three permanent conduits (stdin, stdout, stderr) that stay open for the lifetime of the session:
stdin → commands/expressions sent to the backend.
stdout → normal output / results from the backend.
stderr → errors, warnings, diagnostics, or side-channel logging.

Bidirectional flow with proper framing / protocol so the frontend can reliably separate:
Prompts / readiness signals.
Complete responses vs. streaming output.
Error vs. normal output.
Asynchronous messages (e.g., compiler warnings that arrive later).

The narrative layer (the "REPL" part) handles:
User input (Haskeline / linenoise / brick / etc.).
Command dispatching (:load, :type, :quit, custom extensions).
Output rendering, pagination, syntax highlighting.
Session state (history, variable bindings visible to the user, etc.).
Error recovery and restart logic.


This separation makes it much easier to support multiple backends, embed the REPL in different UIs (terminal, web, IDE), test the backend independently, etc.

**Asymmetric I/O design:**
- FIFO for stdin (stateful, blocking-tolerant, agent freindly)
- Append-only file for stdout and stderr (broadcast pattern, agent friendly)

Inverts the subprocess pattern: rather than polling a REPL subprocess, 
the REPL becomes a message bus node. 

---

## Agent and REPL: The Isomorphism

**Hyper** is the final encoding of traced monoidal categories:

```haskell
newtype Hyper a b = Hyper { invoke :: Hyper b a -> b }
```

To produce a `b`, you invoke your dual (a `Hyper b a`) and use its result. The 
feedback channel is structural in the type, not explicit. `run h = invoke h (Hyper run)` 
ties the knot — the recursive fixed point where the hyperfunction invokes itself.

**Agent** is Hyper materialized as an operational pattern:

```haskell
newtype Agent = Agent { step :: Path -> (Text, Agent) }
```

Where `Path = [Text]` is the append-only history. The agent is a coinductive closure: 
given history, produce next output and next agent. The path *is* the unfolding of the 
recursion.

### The Isomorphism to REPL

A REPL is an Agent in `Kleisli IO`:

```haskell
newtype Repl  = Repl  { step :: Text -> IO (Text, Repl)  }
newtype Agent = Agent { step :: Path -> (Text, Agent)     }
```

Identical shape. Same coinductive pattern. `IO` effects are the monoidal context, 
nothing more. The REPL process *is* the Agent; the named pipe *is* the Path.

### Final Encoding = Extensional Equivalence

Hyper is the *final* encoding because it's the canonical coalgebraic form where 
behavioral equivalence defines equality.

Two agents with the same `Text` trace but different internal state (different module 
loads, say) are the **same agent** in Hyper — because you can only observe their 
outputs. The final encoding makes this equivalence structural, not a choice.

**Answer: yes, they are the same.** Hyper enforces it.

---

## Three Types, One Architecture

```haskell
newtype Agent        = Agent { prompt :: Text -> (Text, Agent) }
newtype Conversation = Conversation [Text]  -- the path taken
type    Memory       = Conversation -> Agent  -- quotient map
```

**Agent**: final encoding — extensional, only behaviour visible.

**Conversation**: Circuit side — the syntax, the history, inspectable constructors.

**Memory**: Galois connection between them. A function mapping paths to behaviours, 
where two paths producing the same future behaviour are identified.

### The Quotient and Compaction

The quotient is **conversations that produce the same next response**: 
`lower . toHyper = lower`. Two `Circuit` terms (conversation histories) 
that agree under `lower` (same agent behaviour going forward) are identified in `Hyper`.

**Compaction is just computing that quotient eagerly rather than lazily.**

The memory model question becomes: **what is the minimal `Conversation` 
that produces this `Agent`?**

That's a compression problem with a categorical answer — the smallest history that 
determines current behaviour. Context window limits force this quotient whether you 
want it or not. Every frontier lab independently discovers that memory is really a 
compaction problem dressed up in different terms.

### Grepl Instantiation

```haskell
type ReplHistory = Conversation  -- the session log, already in .md files
type ReplAgent   = Agent         -- the current GHCi state
```

Grepl is already computing this. The markdown log *is* the `Conversation`. 
The live GHCi process *is* the `Agent`. The question of which log entries to replay 
to reconstruct a given GHCi state is exactly the minimal conversation problem.

### Compact Closed: Cup and Cap

The dual of a `Conversation` is a **summary** — the minimal `Text` that, when used 
as a prompt, produces an equivalent `Agent`.

```
cup  :: () -> Conversation ⊗ Summary    -- expand a summary into a conversation
cap  :: Summary ⊗ Conversation -> ()     -- compress a conversation into a summary
```

The quotient makes cup and cap well-defined rather than lossy. Without it, 
compression loses information; with the quotient, you're just removing redundancy 
that doesn't affect behaviour.

---

## The Architecture Stack

```
Circuit (->) (,) Text Text     -- pure conversation circuit
        ↓ Kleisli IO
Circuit (Kleisli IO) (,) Text Text  -- effectful, PTY-backed
        ↓ lower
Pty -> IO (Text, Pty)          -- the actual REPL step
```

Each layer is a refinement: from abstract to concrete, from pure to effectful.

### PTY as Agent

```haskell
writePty pty "1 + 1\n" >> readPty pty
```

This is **indistinguishable from a minimal LLM harness**:

- PTY = agent
- Terminal = context window
- `spawnCabalRepl` = `newSession`
- `writePty` = token push
- `readPty` = token stream

The only missing piece: **prompt boundary detection**.

### Closing the Cup: Prompt Detection

Without knowing when GHCi has finished responding, you can't compose steps. 
You have the PTY but not the circuit step.

```haskell
readUntilPrompt :: Pty -> IO Text
```

This collects output until it sees `ghci> `, which is exactly the role of `[DONE]` 
in an LLM stream. It closes the cup by delimiting the response boundary.

With `readUntilPrompt`, the step function becomes compositional:

```haskell
step :: Text -> Pty -> IO (Text, Pty)
step cmd pty = do
  writePty pty (cmd <> "\n")
  resp <- readUntilPrompt pty
  return (resp, pty)
```

Now you have a well-defined circuit step that composes.

---

## Streaming and Tagged Text

Agents produce **streams of tagged text**, not atomic responses. Producers and consumers 
work asynchronously — you don't have to wait for the full stream.

```haskell
data Tag = User | Assistant | Thought | Tool | Done

type Tagged = (Tag, Text)
```

### The Step Function with Streaming

Not atomic:
```haskell
step :: Text -> IO [Tagged]  -- wrong: you have to wait
```

But streaming:
```haskell
step :: Tagged -> IO (Stream Tagged)  -- producer yields incrementally
```

Where `Ready` signals that the REPL is waiting for input, not that effects have completed. 
The stream is genuinely open-ended.

### Streaming is Not Request-Response

`ghci> ` is not `Done` — it's `Ready`. A `launchMissile :: IO ()` could still be printing 
to the PTY long after `ghci> ` appears.

This breaks the simple cup/cap handshake. The cap can't fire on `Ready` because the emitter 
might still be producing. **The cap is always a policy decision, not a structural one.**

The consumer decides when to stop reading based on policy:

```haskell
readWithTimeout :: TChan Tagged -> Int -> IO [Tagged]
-- collect until <timeout> microseconds of silence after Ready
```

### TChan is Necessary

The PTY reader must run forever on its own thread, tagging chunks as they arrive. 
You can't consume inline without losing interleaved effects. The `TChan` decouples 
producer (PTY reader) from consumer (orchestrator policy).

The orchestrator can intercept `Tool` and `Thought` tags **in flight**, watching 
the stream without committing to a response boundary.

### Grepl Watcher Already Implements This

```
FSNotify (file modification) 
  → TChan (stream of events)
  → Tag filters (drop stderr, select stdout)
  → Consumer (reads lazily with policy)
```

You built the streaming consumer before the streaming producer. The watcher architecture 
is already the pattern.

### Agent as Hyper

With streaming:

```haskell
newtype Agent = Agent { step :: Tagged -> IO (Stream Tagged) }
```

This is `Hyper Tagged Tagged` in `Kleisli IO`:

```haskell
Hyper a b = Hyper { invoke :: Hyper b a -> b }

-- specialised to Tagged, in Kleisli IO:
Agent = Agent { step :: Tagged -> IO (Stream Tagged) }
--                        ^a         ^b = Stream a (coinductive)
```

The next `Agent` is implicit in the stream — it's the tail after the consumer decides to cap.

### LLM and REPL Isomorphism (With Caveats)

```haskell
-- LLM step
Tagged -> IO (Stream Tagged)  -- policy decides when to cap

-- REPL step  
Tagged -> IO (Stream Tagged)  -- policy decides when to cap
```

Same type. The difference is policy and async communication, not structure. 
Both are effect-restricted, bidirectional, open-ended streams. **The REPL is not 
a request-response protocol. It just looks like one.**

---

## Streams as Hyperfunctions

Don't reach for `StreamT IO`. Model it with `Hyper` directly.

### A Stream is Hyper a a

A stream is a hyperfunction where input and output are the same type:

```haskell
Hyper a a = Hyper { invoke :: Hyper a a -> a }
```

To get the next element:
```haskell
head' :: Hyper a a -> a
head' h = invoke h h
```

The stream *is* the hyperfunction. Each element is produced by invoking the dual, 
which is itself. Self-duality models self-reference.

### Full Exchange Type

An exchange produces a stream of `Tagged` values:

```haskell
Hyper Tagged (Hyper Tagged Tagged)
```

Unpacking:
```haskell
invoke :: Hyper (Hyper Tagged Tagged) Tagged -> Hyper Tagged Tagged
```

To produce a `Hyper Tagged Tagged` (the response stream), you invoke with the dual — 
a consumer that consumes the stream. The producer and consumer are **coupled from the start**.
Neither exists without the other. That's the cup.

### Two-Level Resolution

```haskell
newtype Exchange = Exchange { step :: Exchange -> Agent }
newtype Agent    = Agent    { invoke :: Agent -> Tagged }
```

Two levels of `Hyper`:
- **Outer** (`Exchange`): the session level. One exchange step produces an Agent (the response stream).
- **Inner** (`Agent`): the streaming level. An Agent invokes itself to produce `Tagged` elements.

`Ready` is a `Tag` value, not a type constructor. The policy layer interprets it: 
the orchestrator decides when to cap the exchange based on timeout, semantics, or other criteria. 
The tag is **information for the policy**, not a structural termination signal.

### Why This Works

No separate stream type. No `StreamT IO`. Just hyperfunctions at two levels of recursion:

```haskell
-- One exchange:
Exchange -> Agent

-- The stream of one response:
Agent -> Tagged

-- Composed: full conversation
Exchange -> (Agent -> Tagged)
```

`Circuit` composition naturally follows. The response stream from one exchange becomes 
the input producer for the next. Feedback and coinduction are structural.

---

## The Core Type and Lib

The actual core type you want:

```haskell
Hyper Tagged (IO Tagged)
```

Where `IO` is the effect monad and `Tagged` is the carrier. To produce an `IO Tagged`, 
you invoke with the dual: something that consumes `IO Tagged` and produces `Tagged`.

The static syntax:

```haskell
Circuit (Kleisli IO) t Tagged Tagged
```

The tensor choice `t` encodes the merge strategy:
- `(,)` for parallel agents holding hands
- `Either` for coroutine agents taking turns
- Sequential composition for one agent's `Done` feeding the next agent's `User`

Filtering is a `Loop` that pinches off `Thought` and `Tool` tags while keeping them 
in the feedback channel.

### One-Object Category: Lib

```haskell
Hyper (Hyper Tagged Tagged) Tagged
```

This is the beast. A one-object category:
- **Object**: `Tagged`
- **Morphisms**: `Hyper Tagged Tagged` (agents)
- **2-cells**: `Hyper (Hyper Tagged Tagged) Tagged` (morphisms between morphisms)

This is **Lib** — the infinite library of Text and arrows between them. Agents are paths 
through Lib. Conversations are paths through paths.

### Compact Closed on Lib

```haskell
-- cup: spawn agent and its dual context
cup :: () -> (Hyper Tagged Tagged, Hyper Tagged Tagged)

-- cap: evaluate agent against dual with a policy, collapse to Tagged  
cap :: (Hyper Tagged Tagged, Hyper Tagged Tagged) -> TaggedPolicy -> Tagged
```

Between cup and cap lives all of Lib — the infinite coinductive space of everything 
that can be said. The policy determines when the cap fires, which may be long after `Ready`.

### Composition is Conversation

Two agents compose by one's output becoming the other's input. Associativity law: 
it doesn't matter how you bracket a conversation. The meaning is in the path through Lib, 
not the bracketing.

### Deck Notation is Lib Navigation

You've been building Lib notation all along:

```
lead ⟜ elaboration
```

- **Lead** = a point in Lib (compressed state)
- **Elaboration** = a path to a nearby point (expanded context)
- **⟜** = witness to the arrow between them (the metric distance)

The deck language is literally navigation through Lib. Every card is a position. 
Every connection is a path. The one-object category is where all of this lives.

---
