# `zmq`

`zmq` is an opinionated, high-level wrapper around `libzmq-4.3.4`. It aims to expose a safe and minimal interface that
incorporates `libzmq` best-practices.

Users already familiar with the `libzmq` API may instead find the `libzmq` Haskell library, atop which `zmq` is built,
more comfortable to use, as it exports an unopinionated 1:1 mapping.

## Behavior differences between this library and the original C++ library that it wraps

#### No context type

There is no context type corresponding to `zmq_ctx_t` that the user must create, destroy, and provide whenever opening a
socket. Intead, this library uses a global context stored in a top-level `IORef`.

The global context must be initialized; the user is responsible for wrapping all other calls to this library by a call
to `Zmq.run`, as in:

```haskell
import Zmq qualified

main :: IO ()
main =
  Zmq.run Zmq.defaultOptions do
    ...
```

Failure to do so will result in a runtime exception that says something like, "context not initialized".

The context is terminated automatically at the end of `Zmq.run`. It is always created with `ZMQ_BLOCKY`, so there is no
need (and indeed no way) to set `ZMQ_LINGER` values for all open sockets - they effectively have linger set to 0.

#### No need to close sockets

Sockets are automatically closed. There is no need to acquire a socket in bracket-style and incur a syncactic indent:
just create a socket and use it with straight-line syntax. If you drop all references to a socket, it will be closed by
a Haskell finalizer after garbage collection.

#### Ctrl+C works

By default, GHC will translate one Ctrl+C call to an async exception called `UserInterrupt` that is delivered to the
main thread.

So long as you engineer your program to (effectively) receive this async exception in all threads that have opened a
socket (such as by using a structured concurrency library like [`ki`](https://hackage.haskell.org/package/ki), then all
blocking ZMQ operations that are currently underway will immediately return `EINTR`.

#### Various nonsensical `send` and `receive` operations are prohibited

It's not possible to receive a message on a `Pusher` socket, for example, so this library doesn't allow you to even try.
We don't allow setting `ZMQ_SNDHWM` on sockets that can't send. That sort of thing.

Similarly, it's not possible to send or receive a single-frame message on a `Router` socket, because of the identity
frame.

#### Many sockets are thread-safe

ZMQ sockets are not thread-safe, mostly due to the existence of multiframe messages. This library guards usage
of sockets by a mutex and masks asynchronous exceptions when sending or receiving multiframe messages, so sending and
receiving is thread-safe and exception-safe.

Two exceptions: `Requester` (`REQ`) and `Replier` (`REP`). These sockets' communication patterns are inherently thread-
unsafe, so their usage is not protected by a mutex.

#### `Requester` (`REQ`) sockets are more usable

`Requester` sockets have `ZMQ_REQ_CORRELATE` and `ZMQ_REQ_RELAXED` set automatically. This makes all outgoing requests
from a `Requester` socket contain a two-frame header: a unique message identifier and the normal empty delimiter frame.
When receiving, messages that do not begin with the last-send unique message identifier and an empty delimiter are
silently ignored. The rigid send-recv-send-recv state machine is no more: it is possible to send, then send again.

Additionally, polling on a `Requester` has much nicer semantics. With raw `libzmq`, it is possible for `zmq_poll` to
report that a `REQ` socket is readable, but `zmq_msg_recv` blocks. This happens whenever a response to an outdated
request is received. With this library, if `poll` reports that a `Requester` socket is readable, then calling `receive`
on it will not block.

#### `Router` sockets have `ZMQ_ROUTER_MANDATORY` set automatically

The default behavior of `libzmq` is to silently drop messages that are not routable, because a peer has disappeared, or
the identity frame is bogus. The `ZMQ_ROUTER_MANDATORY` option instead reports this to the user as an `EHOSTUNREACH`
error. This provides the user with strictly more information, with no downside: the user can ignore this error if they
wish. So, we set `ZMQ_ROUTER_MANDATORY` on all `Router` sockets automatically.
