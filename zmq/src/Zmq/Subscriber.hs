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
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanReceive, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __subscriber__ socket.
--
-- Valid peers: __publisher__, __xpublisher__
newtype Subscriber
  = Subscriber (MVar Zmq_socket)
  deriving stock (Eq)
  deriving (Socket) via (ThreadSafeSocket)
  deriving anyclass
    ( CanReceive,
      Options.CanSetSendQueueSize
    )

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
    socketVar <- Socket.openThreadSafeSocket ZMQ_SUB
    socket <- readMVar socketVar
    Options.setSocketOptions socket ZMQ_SUB options
    pure (Subscriber socketVar)

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

-- | Receive a __topic message__ on a __subscriber__ from any peer (fair-queued).
receive :: Subscriber -> IO (Either Error (ByteString, ByteString))
receive socket =
  catchingOkErrors do
    withSocket socket Socket.receiveTwo
