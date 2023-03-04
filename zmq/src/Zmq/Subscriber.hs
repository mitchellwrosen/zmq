module Zmq.Subscriber
  ( Subscriber,
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

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import Libzmq qualified
import Libzmq.Bindings qualified
import Zmq.Context
import Zmq.Endpoint
import Zmq.Error
import Zmq.Internal.Socket qualified

newtype Subscriber
  = Subscriber (MVar Libzmq.Zmq_socket_t)
  deriving stock (Eq)

with :: forall a. Context -> (Subscriber -> IO (Either Error a)) -> IO (Either Error a)
with =
  coerce @(Context -> (MVar Libzmq.Zmq_socket_t -> IO (Either Error a)) -> IO (Either Error a)) Zmq.Internal.Socket.with

bind :: Subscriber -> Endpoint transport -> IO (Either Error ())
bind =
  coerce Zmq.Internal.Socket.bind

unbind :: Subscriber -> Endpoint transport -> IO (Either Error ())
unbind =
  coerce Zmq.Internal.Socket.unbind

connect :: Subscriber -> Endpoint transport -> IO (Either Error ())
connect =
  coerce Zmq.Internal.Socket.connect

disconnect :: Subscriber -> Endpoint transport -> IO (Either Error ())
disconnect =
  coerce Zmq.Internal.Socket.disconnect

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
subscribe :: Subscriber -> ByteString -> IO (Either Error ())
subscribe (Subscriber socketVar) prefix =
  withMVar socketVar \socket ->
    Zmq.Internal.Socket.setByteStringOption socket Libzmq.Bindings._ZMQ_SUBSCRIBE prefix

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
unsubscribe :: Subscriber -> ByteString -> IO (Either Error ())
unsubscribe (Subscriber socketVar) prefix =
  withMVar socketVar \socket ->
    Zmq.Internal.Socket.setByteStringOption socket Libzmq.Bindings._ZMQ_UNSUBSCRIBE prefix

receive :: Subscriber -> IO (Either Error (NonEmpty ByteString))
receive (Subscriber socketVar) =
  withMVar socketVar Zmq.Internal.Socket.receive
