# supervised-state-machines

An experimental implementation of Erlang/OTP's `gen_server` and `supervisor`
behaviours that doesn't use lightweight threads and message passing.

## Motivation

What exactly is it that makes Erlang suitable for writing reliable distributed
systems?

I've previously
[argued](https://github.com/stevana/armstrong-distributed-systems/blob/main/docs/erlang-is-not-about.md#erlangs-not-about-lightweight-processes-and-message-passing)
that it's Erlang's *behaviours* rather than its lightweight processes and
message passing.

Behaviours can be thought of as generic building blocks for building reliable
distributed systems. Erlang/OTP exposes six behaviours and encourages its users
to compose them into bigger systems. The behaviours are generic in that they are
parametrised by interfaces, the idea being that the user implements the
interface in a problem specific way and then the user gets the generic component
from Erlang/OTP. Typically the interface requires a sequential implementation
while the generic component exposes a concurrent (or thread-safe) API, i.e.
behaviours abstract away the low-level and difficult concurrent code which is
difficult to get right. Joe Armstrong
[describes](https://dl.acm.org/doi/10.1145/1238844.1238850) them as follows:

> Behaviors in Erlang can be thought of as parameterizable higher-order parallel
> processes. They represent an extension of conventional higher-order functions
> (like map, fold etc) into a concurrent domain.

Which I think is a good analogy, as e.g. `map` and `fold` hide the low-level
details of `for`-loops, although the concurrent details of behaviours are
typically more complicated than manually dealing with index variables.

This repo is an experiment in trying to implement two of these behaviours,
namely `gen_server` and `supervisor`, without using lightweight
processes/threads and message passing. I believe the last part about not using
lightweight threads is a design space that hasn't been explored much yet. Most
programming languages or libraries seem to start with the assumption that what
makes Erlang great for writing reliable distributed systems is its lightweight
threads and message passing, and they never even get to the point where they
steal the structure of behaviours!

## How it works

### Generic server

The sequential semantics (or "business logic") of a generic server
(`gen_server`) should take some input and the current state and produce some
output and a new updated state, i.e.:

```
  input -> state -> (state, output)
```

Client requests to the server will come in via the network, so we also need a
`Codec` to be able to decode `ByteString`s into `input`s and encode `output`s
into `ByteString`s to be able to reply to the client. We might also want to
deserialise the initial state `state` from disk on startup and serialise it to
disk on termination. See the `StateMachine` [module](src/StateMachine.hs) for
the details of the above.

### Supervisor

The job of a supervisor is to monitor its children for failures and do restarts
according to some predetermined restart strategy in case a failure happens.

Supervisors are organised in trees where generic servers (or more generally any
other worker behaviours) are at the leaves and other supervisors are at the
nodes. Since supervisors trees determine an order (depth-first) they can be used
to deploy a system of generic servers.

See the `Supervisor` [module](src/Supervisor.hs) for details.

### Event loop

The concurrent part of the generic servers is implemented in the `EventLoop`
[module](src/EventLoop.hs). The basic idea is that we concurrently write client
request `ByteString`s to a concurrent queue and the event loop will decode the
input and `step` the right server with said input and respond to the client with
the output produce by the server.

The behavior of supervisors is also implemented in the event loop. Basically we
wrap the `step` function in a `try` and `catch` and in case of failure we do the
appropriate restarts.

### Example

As an example of generic server I've implemented a simple key value store in the
`Example.KeyValueStore` [module](src/Example/KeyValueStore.hs). In
[`app/Main.hs`](app/Main.hs) we start an event loop with a simple supervisor
tree containing the key value store:

```haskell
    main :: IO ()
    main = do
      let sup = Supervisor OneForOne
                  [ Worker ("kv1", kvStore)
                  , Supervisor RestForOne
                      [ Worker ("kv2", kvStore), Worker ("kv3", kvStore) ]
                  ]
      queue <- newTBQueueIO 128
      withEventLoop sup queue $ do
        call_ "kv2" (Store "x" 1) queue
        r0 <- call "kv2" (Lookup "x") queue
        print r0
        call_ "kv2" (Lookup "crash") queue -- Deliberate bug which causes a crash.
        r1 <- call "kv2" (Lookup "x") queue
        print r1
        r2 <- call "kv2" (Lookup "y") queue
        print r2
```

When run with `cabal run kv` it produces the following output:

```
    Calling kv2: Store "x" 1
    KV store starting: kv1
    KV store starting: kv2
    KV store starting: kv3
    Calling kv2: Lookup "x"
    Right "Result (Just 1)"
    Calling kv2: Lookup "crash"
    kv2 threw: divide by zero
    KV store terminating: kv2
    KV store terminating: kv3
    KV store starting: kv2
    KV store starting: kv3
    Calling kv2: Lookup "x"
    Right "Result Nothing"
    Calling kv2: Lookup "y"
    Right "Result Nothing"
```

## Contributing

There are many ways in which this repo can be extended, here are some ideas:

- [ ] Add HTTP endpoint for writing to the event loop queue. (Hint: see the
      `HttpServer` and `EventLoop` modules of this
      [repo](https://dl.acm.org/doi/10.1145/1238844.1238850)));
- [ ] Save and restore the state of the example to disk in `terminate` and
      `init`;
- [ ] Customisable shutdown grace time;
- [ ] The supervisors itself should fail if its children have failed too many
      times within some time interval;
- [ ] Supervisors should be able to supervise supervisor trees that are deployed
      on other computers.

## See also

* There are a handful of supervisor implementations in Haskell
  [already](https://hackage.haskell.org/packages/search?terms=supervisor), but I
  think all of them assume that the children are running on their own threads.
