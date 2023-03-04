module Zmq.XSubscriber
  ( XSubscriber,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    subscribe,
    unsubscribe,
    receive,
    canSend,
    canReceive,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty as List (NonEmpty)
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error)
import Zmq.Internal.Socket (CanReceive, CanSend, Event, Socket, ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket
import Zmq.SubscriptionMessage (SubscriptionMessage (Subscribe, Unsubscribe))
import Zmq.SubscriptionMessage qualified as SubscriptionMessage

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
  coerce (Socket.openThreadSafeSocket ZMQ_XSUB)

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

-- | Subscribe an __xsubscriber__ to a topic (prefix matching).
--
-- To subscribe to all topics, subscribe to the empty string.
subscribe :: XSubscriber -> ByteString -> IO (Either Error ())
subscribe xsubscriber prefix =
  send xsubscriber (Subscribe prefix)

-- | Unsubscribe an __xsubscriber__ from a previously-subscribed topic.
unsubscribe :: XSubscriber -> ByteString -> IO (Either Error ())
unsubscribe xsubscriber prefix =
  send xsubscriber (Unsubscribe prefix)

send :: XSubscriber -> SubscriptionMessage -> IO (Either Error ())
send (XSubscriber socketVar) message =
  withMVar socketVar \socket ->
    Socket.send1 socket (SubscriptionMessage.serialize message)

-- | Receive a __message__ on an __xsubscriber__ from any peer (fair queueing).
receive :: XSubscriber -> IO (Either Error (List.NonEmpty ByteString))
receive (XSubscriber socketVar) =
  withMVar socketVar Socket.receive

-- | /Alias/: 'Zmq.canSend'
canSend :: XSubscriber -> a -> Event a
canSend =
  Socket.canSend

-- | /Alias/: 'Zmq.canReceive'
canReceive :: XSubscriber -> a -> Event a
canReceive =
  Socket.canReceive
