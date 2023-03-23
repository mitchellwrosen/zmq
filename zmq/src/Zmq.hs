module Zmq
  ( -- * Main
    run,

    -- ** Main options
    Options.ioThreads,
    Options.maxSockets,

    -- * Socket
    Socket.Socket,
    monitor,

    -- ** Options
    Options.curveClient,
    Options.curveServer,
    Options.lossy,
    Options.name,
    Options.sendQueueSize,

    -- ** Peering
    Socket.bind,
    Socket.unbind,
    Socket.connect,
    Socket.disconnect,

    -- ** Messaging
    send,
    receive,
    receives,

    -- ** IO multiplexing
    Sockets,
    Ready (..),
    the,
    also,
    poll,
    pollFor,
    pollUntil,

    -- * Socket types
    Dealer,
    Pair,
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
    Options.Options,
    Options.defaultOptions,

    -- * Errors
    Error (..),
    Zmq_error (..),

    -- * Socket subclasses
    Socket.CanSend,
    Socket.CanReceive,
    Socket.CanReceives,
    CanPoll,

    -- ** Options
    Options.CanSetLossy,
    Options.CanSetSendQueueSize,

    -- * Version
    version,
  )
where

import Data.ByteString (ByteString)
import Libzmq
import Zmq.Dealer (Dealer)
import Zmq.Error (Error (..))
import Zmq.Internal.Context
import Zmq.Internal.Curve
import Zmq.Internal.Monitor (monitor)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll, Ready (..), Sockets, also, poll, pollFor, pollUntil, the)
import Zmq.Internal.Socket qualified as Socket
import Zmq.Pair (Pair)
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

send :: Socket.CanSend socket => socket -> ByteString -> IO (Either Error ())
send =
  Socket.send_

receive :: Socket.CanReceive socket => socket -> IO (Either Error ByteString)
receive =
  Socket.receive_

receives :: Socket.CanReceives socket => socket -> IO (Either Error [ByteString])
receives =
  Socket.receives_

version :: (Int, Int, Int)
version =
  zmq_version
