module Zmq
  ( -- * Main
    run,

    -- ** Main options
    ioThreads,
    maxSockets,

    -- * Socket
    Socket,
    open,

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

    -- ** Options
    lossy,
    name,
    sendQueueSize,

    -- * Socket types
    Dealer,
    Publisher,
    Puller,
    Pusher,
    Replier,
    Requester,
    Router,
    Subscriber,
    XPublisher,
    XSubscriber,

    -- * Subscription message
    pattern Subscribe,
    pattern Unsubscribe,

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
import Zmq.Internal.Options
  ( CanSetLossy,
    CanSetSendQueueSize,
    Options,
    defaultOptions,
    ioThreads,
    lossy,
    maxSockets,
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
import Zmq.Publisher (Publisher)
import Zmq.Puller (Puller)
import Zmq.Pusher (Pusher)
import Zmq.Replier (Replier)
import Zmq.Requester (Requester)
import Zmq.Router (Router)
import Zmq.Subscriber (Subscriber)
import Zmq.Subscription (pattern Subscribe, pattern Unsubscribe)
import Zmq.XPublisher (XPublisher)
import Zmq.XSubscriber (XSubscriber)

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
