module Zmq
  ( -- * Main
    run,
    Options (..),
    defaultOptions,

    -- * Socket
    Socket,
    bind,
    unbind,
    connect,
    disconnect,

    -- * IO multiplexing
    Event,
    canSend,
    canReceive,
    poll,

    -- * Socket types
    Publisher,
    Puller,
    Pusher,
    Requester,
    Responder,
    Subscriber,
    XPublisher,
    XSubscriber,

    -- * Subscription message
    pattern Subscribe,
    pattern Unsubscribe,

    -- * Errors
    Error (..),
    Zmq_error (..),

    -- * Version
    version,
  )
where

import Libzmq (Zmq_error (..), zmq_version)
import Zmq.Error (Error (..))
import Zmq.Internal.Context
import Zmq.Internal.Options (Options (..), defaultOptions)
import Zmq.Internal.Socket (Event, Socket, bind, canReceive, canSend, connect, disconnect, poll, unbind)
import Zmq.Publisher (Publisher)
import Zmq.Puller (Puller)
import Zmq.Pusher (Pusher)
import Zmq.Requester (Requester)
import Zmq.Responder (Responder)
import Zmq.Subscriber (Subscriber)
import Zmq.Subscription (pattern Subscribe, pattern Unsubscribe)
import Zmq.XPublisher (XPublisher)
import Zmq.XSubscriber (XSubscriber)

version :: (Int, Int, Int)
version =
  zmq_version
