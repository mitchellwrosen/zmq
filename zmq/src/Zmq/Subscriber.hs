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
import Zmq.Endpoint
import Zmq.Error
import Zmq.Internal.Socket qualified as Socket

newtype Subscriber
  = Subscriber (MVar Zmq_socket)
  deriving stock (Eq)

open :: IO (Either Error Subscriber)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_SUB)

bind :: Subscriber -> Endpoint transport -> IO (Either Error ())
bind (Subscriber socketVar) endpoint =
  withMVar socketVar \socket -> Socket.bind socket endpoint

unbind :: Subscriber -> Endpoint transport -> IO (Either Error ())
unbind (Subscriber socketVar) endpoint =
  withMVar socketVar \socket -> Socket.unbind socket endpoint

connect :: Subscriber -> Endpoint transport -> IO (Either Error ())
connect (Subscriber socketVar) endpoint =
  withMVar socketVar \socket -> Socket.connect socket endpoint

disconnect :: Subscriber -> Endpoint transport -> IO (Either Error ())
disconnect (Subscriber socketVar) endpoint =
  withMVar socketVar \socket -> Socket.disconnect socket endpoint

subscribe :: Subscriber -> ByteString -> IO (Either Error ())
subscribe (Subscriber socketVar) prefix =
  withMVar socketVar \socket ->
    Socket.setOption socket Libzmq.ZMQ_SUBSCRIBE prefix

unsubscribe :: Subscriber -> ByteString -> IO (Either Error ())
unsubscribe (Subscriber socketVar) prefix =
  withMVar socketVar \socket ->
    Socket.setOption socket Libzmq.ZMQ_UNSUBSCRIBE prefix

receive :: Subscriber -> IO (Either Error (List.NonEmpty ByteString))
receive (Subscriber socketVar) =
  withMVar socketVar Socket.receive
