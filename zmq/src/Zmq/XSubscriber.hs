module Zmq.XSubscriber
  ( XSubscriber,
    defaultOptions,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    subscribe,
    unsubscribe,
    send,
    sends,
    receive,
    receives,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error, catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll)
import Zmq.Internal.Socket (CanReceive, CanSend, Socket (withSocket), ThreadSafeSocket (..))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Subscription (pattern Subscribe, pattern Unsubscribe)

-- | A thread-safe __xsubscriber__ socket.
--
-- Valid peers: __publisher__, __xpublisher__
newtype XSubscriber
  = XSubscriber ThreadSafeSocket
  deriving stock (Eq)
  deriving anyclass (CanPoll)
  deriving (Socket) via (ThreadSafeSocket)

instance CanReceive XSubscriber where
  receive_ = receive

instance CanSend XSubscriber where
  send_ = send

defaultOptions :: Options XSubscriber
defaultOptions =
  Options.defaultOptions

-- | Open an __xsubscriber__.
open :: Options XSubscriber -> IO (Either Error XSubscriber)
open options =
  catchingOkErrors do
    socketVar <- Socket.openThreadSafeSocket ZMQ_XSUB
    socket <- readMVar socketVar
    Options.setSocketOption socket ZMQ_SNDHWM 0 -- don't drop subscriptions
    Options.setSocketOptions socket ZMQ_XSUB options
    pure (XSubscriber (ThreadSafeSocket socketVar (Options.optionsName options)))

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
-- This operation never blocks. All peers with full messages queues will not receive the message.
--
-- /Alias/: 'Zmq.send'
send :: XSubscriber -> ByteString -> IO (Either Error ())
send socket0 frame =
  catchingOkErrors do
    withSocket socket0 \socket ->
      Socket.sendOneWontBlock socket (Socket.socketName socket0) frame False

-- | Send a __multiframe message__ on an __xsubscriber__ to all peers.
--
-- This operation never blocks. All peers with full messages queues will not receive the message.
sends :: XSubscriber -> [ByteString] -> IO (Either Error ())
sends socket0 = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      withSocket socket0 \socket ->
        Socket.sendManyWontBlock socket (Socket.socketName socket0) (frame :| frames)

-- | Receive a __message__ on an __xsubscriber__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: XSubscriber -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on an __xsubscriber__ from any peer (fair-queued).
receives :: XSubscriber -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
