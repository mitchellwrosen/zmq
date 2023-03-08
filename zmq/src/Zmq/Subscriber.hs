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
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll)
import Zmq.Internal.Socket (CanReceive, CanReceives, Socket (withSocket), ThreadSafeSocket (..))
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __subscriber__ socket.
--
-- Valid peers: __publisher__, __xpublisher__
newtype Subscriber
  = Subscriber (ThreadSafeSocket)
  deriving stock (Eq)
  deriving (Socket) via (ThreadSafeSocket)
  deriving anyclass
    ( CanPoll,
      Options.CanSetSendQueueSize
    )

instance CanReceive Subscriber where
  receive_ = receive

instance CanReceives Subscriber where
  receives_ = receives

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
    socket@(ThreadSafeSocket _ zsocket _) <- Socket.openThreadSafeSocket ZMQ_SUB (Options.optionsName options)
    Options.setSocketOption zsocket ZMQ_SNDHWM 0 -- don't drop subscriptions
    Options.setSocketOptions zsocket ZMQ_SUB options
    pure (Subscriber socket)

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
subscribe socket0 prefix =
  catchingOkErrors do
    withSocket socket0 \socket ->
      Options.setSocketOption socket Libzmq.ZMQ_SUBSCRIBE prefix

-- | Unsubscribe a __subscriber__ from a previously-subscribed __topic__.
unsubscribe :: Subscriber -> ByteString -> IO (Either Error ())
unsubscribe socket0 prefix =
  catchingOkErrors do
    withSocket socket0 \socket ->
      Options.setSocketOption socket Libzmq.ZMQ_UNSUBSCRIBE prefix

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
