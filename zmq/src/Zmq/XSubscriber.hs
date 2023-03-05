module Zmq.XSubscriber
  ( XSubscriber,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    subscribe,
    unsubscribe,
    send,
    receive,
    canSend,
    canReceive,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error, catchingOkErrors)
import Zmq.Internal.Socket (CanReceive, CanSend, Event, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket
import Zmq.Subscription (pattern Subscribe, pattern Unsubscribe)

-- | A thread-safe __xsubscriber__ socket.
--
-- Valid peers: __publisher__, __xpublisher__
newtype XSubscriber
  = XSubscriber (MVar Zmq_socket)
  deriving stock (Eq)
  deriving (Socket) via (ThreadSafeSocket)

instance CanSend XSubscriber

instance CanReceive XSubscriber

-- | Open an __xsubscriber__.
open :: IO (Either Error XSubscriber)
open =
  coerce (catchingOkErrors (Socket.openThreadSafeSocket ZMQ_XSUB))

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
-- If a peer has a full message queue, it will not receive the message.
send :: XSubscriber -> ByteString -> IO (Either Error ())
send socket0 message =
  withSocket socket0 \socket ->
    Socket.send socket message

-- | Receive a __topic message__ on an __xsubscriber__ from any peer (fair-queued).
receive :: XSubscriber -> IO (Either Error (ByteString, ByteString))
receive socket =
  withSocket socket Socket.receive2

-- | /Alias/: 'Zmq.canSend'
canSend :: XSubscriber -> a -> Event a
canSend =
  Socket.canSend

-- | /Alias/: 'Zmq.canReceive'
canReceive :: XSubscriber -> a -> Event a
canReceive =
  Socket.canReceive
