module Zmq
  ( -- * Main
    run,
    Options (..),
    defaultOptions,

    -- * Socket types
    Publisher,
    Puller,
    Pusher,
    Requester,
    Responder,
    Subscriber,
    XPublisher,
    XSubscriber,

    -- * Transport
    Transport (..),
    CompatibleTransport,

    -- * Endpoint
    Endpoint (..),
    inproc,

    -- * Subscription message
    SubscriptionMessage (..),

    -- * Errors
    Error (..),

    -- * Version
    version,
  )
where

import Libzmq (zmq_version)
import Zmq.Endpoint (Endpoint (..), inproc)
import Zmq.Error (Error (..))
import Zmq.Internal (CompatibleTransport, Transport (..))
import Zmq.Internal.Context
import Zmq.Internal.Options (Options (..), defaultOptions)
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
