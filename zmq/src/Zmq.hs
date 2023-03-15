module Zmq
  ( -- * Main
    run,

    -- ** Main options
    ioThreads,
    maxSockets,

    -- * Socket
    Socket,
    open,

    -- ** Options
    curveClient,
    curveServer,
    lossy,
    monitor,
    name,
    sendQueueSize,

    -- ** Peering
    bind,
    unbind,
    connect,
    disconnect,

    -- ** Messaging
    send,
    receive,
    receives,

    -- ** IO multiplexing
    Sockets,
    the,
    also,
    poll,
    pollFor,
    pollUntil,

    -- * Socket types
    Dealer,
    Pub,
    Pull,
    Push,
    Rep,
    Req,
    Router,
    Sub,
    XPub,
    XSub,

    -- * Subscription message
    pattern Subscribe,
    pattern Unsubscribe,

    -- * Encryption
    CurvePublicKey (..),
    CurveSecretKey (..),
    generateCurveSecretKey,
    deriveCurvePublicKey,

    -- * Options
    Options,
    defaultOptions,

    -- * Errors
    Error (..),
    Zmq_error (..),

    -- * Socket subclasses
    CanSend,
    CanReceive,
    CanReceives,
    CanPoll,

    -- ** Options
    CanSetLossy,
    CanSetSendQueueSize,

    -- * Version
    version,
  )
where

import Data.ByteString (ByteString)
import Libzmq (Zmq_error (..), zmq_version)
import Zmq.Dealer (Dealer)
import Zmq.Error (Error (..))
import Zmq.Internal.Context
import Zmq.Internal.Curve
import Zmq.Internal.Options
  ( CanSetLossy,
    CanSetSendQueueSize,
    Options,
    curveClient,
    curveServer,
    defaultOptions,
    ioThreads,
    lossy,
    maxSockets,
    monitor,
    name,
    sendQueueSize,
  )
import Zmq.Internal.Poll (CanPoll, Sockets, also, poll, pollFor, pollUntil, the)
import Zmq.Internal.Socket
  ( CanReceive (receive_),
    CanReceives,
    CanSend (send_),
    Socket (openSocket),
    bind,
    connect,
    disconnect,
    receives_,
    unbind,
  )
import Zmq.Pub (Pub)
import Zmq.Pull (Pull)
import Zmq.Push (Push)
import Zmq.Rep (Rep)
import Zmq.Req (Req)
import Zmq.Router (Router)
import Zmq.Sub (Sub)
import Zmq.Subscription (pattern Subscribe, pattern Unsubscribe)
import Zmq.XPub (XPub)
import Zmq.XSub (XSub)

open :: Socket socket => Options socket -> IO (Either Error socket)
open =
  openSocket

send :: CanSend socket => socket -> ByteString -> IO (Either Error ())
send =
  send_

receive :: CanReceive socket => socket -> IO (Either Error ByteString)
receive =
  receive_

receives :: CanReceives socket => socket -> IO (Either Error [ByteString])
receives =
  receives_

version :: (Int, Int, Int)
version =
  zmq_version
