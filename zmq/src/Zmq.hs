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
    SubscriptionMessage (..),

    -- * Errors
    Error (..),

    -- * Version
    version,
  )
where

import Libzmq (zmq_version)
import Zmq.Error (Error (..))
import Zmq.Internal.Context
import Zmq.Internal.Options (Options (..), defaultOptions)
import Zmq.Internal.Socket (Socket, bind, connect, disconnect, unbind)
import Zmq.Publisher (Publisher)
import Zmq.Puller (Puller)
import Zmq.Pusher (Pusher)
import Zmq.Requester (Requester)
import Zmq.Responder (Responder)
import Zmq.Subscriber (Subscriber)
import Zmq.SubscriptionMessage (SubscriptionMessage (..))
import Zmq.XPublisher (XPublisher)
import Zmq.XSubscriber (XSubscriber)

version :: (Int, Int, Int)
version =
  zmq_version
