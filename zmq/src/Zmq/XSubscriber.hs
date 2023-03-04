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
import Zmq.Internal.Socket qualified as Socket
import Zmq.SubscriptionMessage (SubscriptionMessage (Subscribe, Unsubscribe))
import Zmq.SubscriptionMessage qualified as SubscriptionMessage

newtype XSubscriber
  = XSubscriber (MVar Zmq_socket)
  deriving stock (Eq)

open :: IO (Either Error XSubscriber)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_XSUB)

bind :: XSubscriber -> Endpoint transport -> IO (Either Error ())
bind (XSubscriber socketVar) endpoint =
  withMVar socketVar \socket -> Socket.bind socket endpoint

unbind :: XSubscriber -> Endpoint transport -> IO (Either Error ())
unbind (XSubscriber socketVar) endpoint =
  withMVar socketVar \socket -> Socket.unbind socket endpoint

connect :: XSubscriber -> Endpoint transport -> IO (Either Error ())
connect (XSubscriber socketVar) endpoint =
  withMVar socketVar \socket -> Socket.connect socket endpoint

disconnect :: XSubscriber -> Endpoint transport -> IO (Either Error ())
disconnect (XSubscriber socketVar) endpoint =
  withMVar socketVar \socket -> Socket.disconnect socket endpoint

subscribe :: XSubscriber -> ByteString -> IO (Either Error ())
subscribe xsubscriber prefix =
  send xsubscriber (Subscribe prefix)

unsubscribe :: XSubscriber -> ByteString -> IO (Either Error ())
unsubscribe xsubscriber prefix =
  send xsubscriber (Unsubscribe prefix)

send :: XSubscriber -> SubscriptionMessage -> IO (Either Error ())
send (XSubscriber socketVar) message =
  withMVar socketVar \socket ->
    Socket.send1 socket (SubscriptionMessage.serialize message)

receive :: XSubscriber -> IO (Either Error (List.NonEmpty ByteString))
receive (XSubscriber socketVar) =
  withMVar socketVar Socket.receive
