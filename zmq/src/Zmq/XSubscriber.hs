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
  )
where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty as List (NonEmpty)
import Libzmq
import UnliftIO
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Context qualified as Zmq.Internal.Socket
import Zmq.SubscriptionMessage (SubscriptionMessage (..))
import Zmq.SubscriptionMessage qualified as SubscriptionMessage

newtype XSubscriber
  = XSubscriber (MVar Zmq_socket)
  deriving stock (Eq)

open :: IO (Either Error XSubscriber)
open =
  coerce (Zmq.Internal.Socket.openThreadSafeSocket ZMQ_XSUB)

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

receive :: XSubscriber -> IO (Either Error (List.NonEmpty ByteString))
receive (XSubscriber socketVar) =
  withMVar socketVar Zmq.Internal.Socket.receive
