module Zmq.XSubscriber
  ( XSubscriber,
    defaultOptions,
    sendQueueSize,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    subscribe,
    unsubscribe,
    send,
    receive,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error, catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanReceive, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket
import Zmq.Subscription (pattern Subscribe, pattern Unsubscribe)

-- | A thread-safe __xsubscriber__ socket.
--
-- Valid peers: __publisher__, __xpublisher__
newtype XSubscriber
  = XSubscriber (MVar Zmq_socket)
  deriving stock (Eq)
  deriving anyclass
    ( CanReceive,
      Options.CanSetSendQueueSize
    )
  deriving (Socket) via (ThreadSafeSocket)

defaultOptions :: Options XSubscriber
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options XSubscriber
sendQueueSize =
  Options.sendQueueSize

-- | Open an __xsubscriber__.
open :: Options XSubscriber -> IO (Either Error XSubscriber)
open options =
  catchingOkErrors do
    socketVar <- Socket.openThreadSafeSocket ZMQ_XSUB
    socket <- readMVar socketVar
    Options.setSocketOptions socket options
    pure (XSubscriber socketVar)

-- | Bind an __xsubscriber__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: XSubscriber -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind an __xsubscriber__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: XSubscriber -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect an __xsubscriber__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: XSubscriber -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect an __xsubscriber__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: XSubscriber -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Subscribe an __xsubscriber__ to a __topic__ (prefix matching).
--
-- To subscribe to all topics, subscribe to the empty string.
subscribe :: XSubscriber -> ByteString -> IO (Either Error ())
subscribe xsubscriber prefix =
  send xsubscriber (Subscribe prefix)

-- | Unsubscribe an __xsubscriber__ from a previously-subscribed __topic__.
unsubscribe :: XSubscriber -> ByteString -> IO (Either Error ())
unsubscribe xsubscriber prefix =
  send xsubscriber (Unsubscribe prefix)

-- | Send a __message__ on an __xsubscriber__ to all peers.
--
-- The message may not begin with the byte @0x00@ or @0x01@.
--
-- This operation never blocks. All peers with full messages queues will not receive the message.
send :: XSubscriber -> ByteString -> IO (Either Error ())
send socket0 message =
  catchingOkErrors do
    withSocket socket0 \socket ->
      Socket.sendWontBlock socket message

-- | Receive a __topic message__ on an __xsubscriber__ from any peer (fair-queued).
receive :: XSubscriber -> IO (Either Error (ByteString, ByteString))
receive socket =
  catchingOkErrors do
    withSocket socket Socket.receiveTwo
