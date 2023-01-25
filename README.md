# supervised-state-machines

An experiment in implementing Erlang-style supervisor trees on a single thread.

## Background

* What exactly is it that makes Erlang suitable for writing reliable distributed systems?

* I've previously
  [argued](https://github.com/stevana/armstrong-distributed-systems/blob/main/docs/erlang-is-not-about.md#erlangs-not-about-lightweight-processes-and-message-passing)
  that it's Erlang's behaviours rather than its lightweight processes and
  message passing

* Erlang behaviours are merely interfaces, so we should be able to implement
  them in many programming languages

* For example the `gen_server` worker behaviour's implementation can be seen as
  a state machine of type `Input -> State -> (State, Output)`

* And the `supervisor` behaviour as a try-catch wrapper around the step function
  of said state machine

## How it works

* Supervisors
* KV store example
* `cabal run kv`

## Contributing

- [ ] Save and restore state to disk
- [ ] Customisable shutdown grace time
- [ ] Failing supervisors
- [ ] Remote supervisors

## See also

* There are a handful of supervisor implementations in Haskell
  [already](https://hackage.haskell.org/packages/search?terms=supervisor), but I
  think all of them assume that the children are running on their own threads.
