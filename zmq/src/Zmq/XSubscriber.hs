module Zmq.XSubscriber
  ( XSubscriber,
    open,
    subscribe,
    unsubscribe,
    receive,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty as List (NonEmpty)
import Libzmq
import Zmq.Error (Error)
import Zmq.Internal.Socket (Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.SubscriptionMessage (SubscriptionMessage (Subscribe, Unsubscribe))
import Zmq.SubscriptionMessage qualified as SubscriptionMessage

newtype XSubscriber
  = XSubscriber (MVar Zmq_socket)
  deriving stock (Eq)

instance Socket XSubscriber where
  withSocket (XSubscriber socketVar) =
    withMVar socketVar

open :: IO (Either Error XSubscriber)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_XSUB)

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
