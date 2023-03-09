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

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error, catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll)
import Zmq.Internal.Socket (CanReceive, CanReceives, CanSend, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadSafeSocket (ThreadSafeSocket)
import Zmq.Internal.ThreadSafeSocket qualified as ThreadSafeSocket
import Zmq.Subscription (pattern Subscribe, pattern Unsubscribe)

-- | A thread-safe __xsubscriber__ socket.
--
-- Valid peers: __publisher__, __xpublisher__
newtype XSubscriber
  = XSubscriber ThreadSafeSocket
  deriving stock (Eq)
  deriving anyclass (CanPoll)

instance CanReceive XSubscriber where
  receive_ = receive

instance CanReceives XSubscriber where
  receives_ = receives

instance CanSend XSubscriber where
  send_ = send

instance Socket XSubscriber where
  openSocket = open
  getSocket = coerce ThreadSafeSocket.raw
  withSocket (XSubscriber socket) = ThreadSafeSocket.with socket
  socketName = coerce ThreadSafeSocket.name

defaultOptions :: Options XSubscriber
defaultOptions =
  Options.defaultOptions

-- | Open an __xsubscriber__.
open :: Options XSubscriber -> IO (Either Error XSubscriber)
open options =
  catchingOkErrors do
    coerce do
      ThreadSafeSocket.open
        ZMQ_XSUB
        ( Options.sockopt ZMQ_SNDHWM 0 -- don't drop subscriptions
            <> options
        )

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
send socket frame =
  catchingOkErrors do
    Socket.sendOneWontBlock socket frame False

-- | Send a __multiframe message__ on an __xsubscriber__ to all peers.
--
-- This operation never blocks. All peers with full messages queues will not receive the message.
sends :: XSubscriber -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      Socket.sendManyWontBlock socket (frame :| frames)

-- | Receive a __message__ on an __xsubscriber__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: XSubscriber -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on an __xsubscriber__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: XSubscriber -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
