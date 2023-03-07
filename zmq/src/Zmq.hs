module Zmq
  ( -- * Main
    run,

    -- ** Main options
    ioThreads,
    maxMessageSize,
    maxSockets,

    -- * Socket
    Socket,
    bind,
    unbind,
    connect,
    disconnect,
    receive,

    -- ** Socket options
    lossy,
    sendQueueSize,

    -- *** Socket options type classes
    CanSetLossy,
    CanSetSendQueueSize,

    -- * IO multiplexing
    Sockets,
    the,
    also,
    poll,

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
    maxMessageSize,
    maxSockets,
    sendQueueSize,
  )
import Zmq.Internal.Socket (CanReceive (receive_), Socket, Sockets, also, bind, connect, disconnect, poll, the, unbind)
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

receive :: CanReceive socket => socket -> IO (Either Error ByteString)
receive =
  receive_

version :: (Int, Int, Int)
version =
  zmq_version
