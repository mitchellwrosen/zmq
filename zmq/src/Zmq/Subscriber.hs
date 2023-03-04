module Zmq.Subscriber
  ( Subscriber,
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

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty as List (NonEmpty)
import Libzmq
import Libzmq.Bindings qualified
import Zmq.Endpoint
import Zmq.Error
import Zmq.Internal.Context qualified as Zmq.Internal.Socket

newtype Subscriber
  = Subscriber (MVar Zmq_socket)
  deriving stock (Eq)

open :: IO (Either Error Subscriber)
open =
  coerce (Zmq.Internal.Socket.openThreadSafeSocket ZMQ_SUB)

bind :: Subscriber -> Endpoint transport -> IO (Either Error ())
bind (Subscriber socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.bind socket endpoint

unbind :: Subscriber -> Endpoint transport -> IO (Either Error ())
unbind (Subscriber socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.unbind socket endpoint

connect :: Subscriber -> Endpoint transport -> IO (Either Error ())
connect (Subscriber socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.connect socket endpoint

disconnect :: Subscriber -> Endpoint transport -> IO (Either Error ())
disconnect (Subscriber socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.disconnect socket endpoint

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

receive :: Subscriber -> IO (Either Error (List.NonEmpty ByteString))
receive (Subscriber socketVar) =
  withMVar socketVar Zmq.Internal.Socket.receive
