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
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Libzmq qualified
import UnliftIO
import Zmq.Context
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Socket qualified
import Zmq.SubscriptionMessage (SubscriptionMessage (..))
import Zmq.SubscriptionMessage qualified as SubscriptionMessage
import Zmqhs qualified

newtype XSubscriber
  = XSubscriber (MVar Libzmq.Zmq_socket_t)
  deriving stock (Eq)

with :: forall a. Context -> (XSubscriber -> IO (Either Error a)) -> IO (Either Error a)
with =
  coerce @(Context -> (MVar Libzmq.Zmq_socket_t -> IO (Either Error a)) -> IO (Either Error a)) Zmq.Internal.Socket.with

bind :: XSubscriber -> Endpoint transport -> IO (Either Error ())
bind =
  coerce Zmq.Internal.Socket.bind

unbind :: XSubscriber -> Endpoint transport -> IO (Either Error ())
unbind =
  coerce Zmq.Internal.Socket.unbind

connect :: XSubscriber -> Endpoint transport -> IO (Either Error ())
connect =
  coerce Zmq.Internal.Socket.connect

disconnect :: XSubscriber -> Endpoint transport -> IO (Either Error ())
disconnect =
  coerce Zmq.Internal.Socket.disconnect

subscribe :: XSubscriber -> ByteString -> IO ()
subscribe xsubscriber prefix =
  send xsubscriber (Subscribe prefix)

unsubscribe :: XSubscriber -> ByteString -> IO ()
unsubscribe xsubscriber prefix =
  send xsubscriber (Unsubscribe prefix)

send :: XSubscriber -> SubscriptionMessage -> IO ()
send (XSubscriber socketVar) message =
  Zmq.Internal.Socket.send socketVar (SubscriptionMessage.serialize message :| [])

receive :: XSubscriber -> IO (NonEmpty ByteString)
receive =
  coerce Zmq.Internal.Socket.receive