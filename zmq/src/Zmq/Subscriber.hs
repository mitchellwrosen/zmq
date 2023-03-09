module Zmq.Subscriber
  ( Subscriber,
    defaultOptions,
    sendQueueSize,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    subscribe,
    unsubscribe,
    receive,
    receives,
  )
where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll)
import Zmq.Internal.Socket (CanReceive, CanReceives, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadSafeSocket (ThreadSafeSocket)
import Zmq.Internal.ThreadSafeSocket qualified as ThreadSafeSocket

-- | A thread-safe __subscriber__ socket.
--
-- Valid peers: __publisher__, __xpublisher__
newtype Subscriber
  = Subscriber (ThreadSafeSocket)
  deriving stock (Eq)
  deriving anyclass
    ( CanPoll,
      Options.CanSetSendQueueSize
    )

instance CanReceive Subscriber where
  receive_ = receive

instance CanReceives Subscriber where
  receives_ = receives

instance Socket Subscriber where
  openSocket = open
  getSocket = coerce ThreadSafeSocket.raw
  withSocket (Subscriber socket) = ThreadSafeSocket.with socket
  socketName = coerce ThreadSafeSocket.name

defaultOptions :: Options Subscriber
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Subscriber
sendQueueSize =
  Options.sendQueueSize

-- | Open a __subscriber__.
open :: Options Subscriber -> IO (Either Error Subscriber)
open options =
  catchingOkErrors do
    coerce do
      ThreadSafeSocket.open
        ZMQ_SUB
        ( Options.sockopt ZMQ_SNDHWM 0 -- don't drop subscriptions
            <> options
        )

-- | Bind a __subscriber__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Subscriber -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __subscriber__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Subscriber -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __subscriber__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Subscriber -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __subscriber__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Subscriber -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Subscribe a __subscriber__ to a __topic__ (prefix matching).
--
-- To subscribe to all topics, subscribe to the empty string.
subscribe :: Subscriber -> ByteString -> IO (Either Error ())
subscribe socket prefix =
  catchingOkErrors do
    withSocket socket do
      Options.setSocketOptions (Socket.getSocket socket) (Options.sockopt ZMQ_SUBSCRIBE prefix)

-- | Unsubscribe a __subscriber__ from a previously-subscribed __topic__.
unsubscribe :: Subscriber -> ByteString -> IO (Either Error ())
unsubscribe socket prefix =
  catchingOkErrors do
    withSocket socket do
      Options.setSocketOptions (Socket.getSocket socket) (Options.sockopt ZMQ_UNSUBSCRIBE prefix)

-- | Receive a __message__ on a __subscriber__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: Subscriber -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __subscriber__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: Subscriber -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
