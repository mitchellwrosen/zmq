module Zmq.XPublisher
  ( XPublisher,
    with,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    receive,
  )
where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Function
import Data.List.NonEmpty (NonEmpty)
import Libzmq qualified
import UnliftIO
import Zmq.Context
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal (renderEndpoint)
import Zmq.Internal.Socket qualified
import Zmq.SubscriptionMessage
import Zmqhs qualified

newtype XPublisher
  = XPublisher (MVar Libzmq.Zmq_socket_t)
  deriving stock (Eq)

with :: forall a. Context -> (XPublisher -> IO (Either Error a)) -> IO (Either Error a)
with =
  coerce @(Context -> (MVar Libzmq.Zmq_socket_t -> IO (Either Error a)) -> IO (Either Error a)) Zmq.Internal.Socket.with

bind :: XPublisher -> Endpoint transport -> IO (Either Error ())
bind =
  coerce Zmq.Internal.Socket.bind

unbind :: XPublisher -> Endpoint transport -> IO (Either Error ())
unbind =
  coerce Zmq.Internal.Socket.unbind

connect :: XPublisher -> Endpoint transport -> IO (Either Error ())
connect =
  coerce Zmq.Internal.Socket.connect

disconnect :: XPublisher -> Endpoint transport -> IO (Either Error ())
disconnect =
  coerce Zmq.Internal.Socket.disconnect

send :: XPublisher -> NonEmpty ByteString -> IO ()
send =
  coerce Zmq.Internal.Socket.send

receive :: XPublisher -> IO SubscriptionMessage
receive (XPublisher socketVar) =
  withMVar socketVar \socket ->
    fix \again ->
      Zmqhs.receive socket >>= \case
        UnsubscribeMessage prefix -> pure (Unsubscribe prefix)
        SubscribeMessage prefix -> pure (Subscribe prefix)
        _ -> again
