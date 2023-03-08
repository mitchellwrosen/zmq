module Zmq
  ( -- * Main
    run,

    -- ** Main options
    ioThreads,
    maxSockets,

    -- * Socket
    Socket,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    receive,
    receives,

    -- ** Socket options
    lossy,
    name,
    sendQueueSize,

    -- * IO multiplexing
    Sockets,
    the,
    also,
    poll,
    pollFor,

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
import Zmq.Internal.Poll (CanPoll, Sockets, also, poll, pollFor, the)
import Zmq.Internal.Socket
  ( CanReceive (receive_),
    CanReceives,
    CanSend (send_),
    Socket,
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
