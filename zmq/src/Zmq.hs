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

    -- ** Socket options
    lossy,

    -- *** Socket options type classes
    CanSetLossy,

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

import Libzmq (Zmq_error (..), zmq_version)
import Zmq.Dealer (Dealer)
import Zmq.Error (Error (..))
import Zmq.Internal.Context
import Zmq.Internal.Options (CanSetLossy, Options, defaultOptions, ioThreads, lossy, maxMessageSize, maxSockets)
import Zmq.Internal.Socket (Socket, Sockets, also, bind, connect, disconnect, poll, the, unbind)
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

version :: (Int, Int, Int)
version =
  zmq_version
