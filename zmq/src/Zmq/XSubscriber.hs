module Zmq.XSubscriber
  ( XSubscriber,
    with,
    bind,
    unbind,
    connect,
    disconnect,
    subscribe,
    unsubscribe,
    receive,
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Libzmq
import UnliftIO
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Socket qualified
import Zmq.SubscriptionMessage (SubscriptionMessage (..))
import Zmq.SubscriptionMessage qualified as SubscriptionMessage

newtype XSubscriber
  = XSubscriber (MVar Zmq_socket)
  deriving stock (Eq)

with :: (XSubscriber -> IO (Either Error a)) -> IO (Either Error a)
with action =
  Zmq.Internal.Socket.with ZMQ_XSUB \socket -> do
    socketVar <- newMVar socket
    action (XSubscriber socketVar)

bind :: XSubscriber -> Endpoint transport -> IO (Either Error ())
bind (XSubscriber socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.bind socket endpoint

unbind :: XSubscriber -> Endpoint transport -> IO (Either Error ())
unbind (XSubscriber socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.unbind socket endpoint

connect :: XSubscriber -> Endpoint transport -> IO (Either Error ())
connect (XSubscriber socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.connect socket endpoint

disconnect :: XSubscriber -> Endpoint transport -> IO (Either Error ())
disconnect (XSubscriber socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.disconnect socket endpoint

subscribe :: XSubscriber -> ByteString -> IO (Either Error ())
subscribe xsubscriber prefix =
  send xsubscriber (Subscribe prefix)

unsubscribe :: XSubscriber -> ByteString -> IO (Either Error ())
unsubscribe xsubscriber prefix =
  send xsubscriber (Unsubscribe prefix)

send :: XSubscriber -> SubscriptionMessage -> IO (Either Error ())
send (XSubscriber socketVar) message =
  withMVar socketVar \socket ->
    Zmq.Internal.Socket.send1 socket (SubscriptionMessage.serialize message)

receive :: XSubscriber -> IO (Either Error (NonEmpty ByteString))
receive (XSubscriber socketVar) =
  withMVar socketVar Zmq.Internal.Socket.receive
