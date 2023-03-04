module Zmq.Subscriber
  ( Subscriber,
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
import Zmq.Error
import Zmq.Internal.Socket (Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket

newtype Subscriber
  = Subscriber (MVar Zmq_socket)
  deriving stock (Eq)

instance Socket Subscriber where
  withSocket (Subscriber socketVar) =
    withMVar socketVar

open :: IO (Either Error Subscriber)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_SUB)

subscribe :: Subscriber -> ByteString -> IO (Either Error ())
subscribe socket0 prefix =
  withSocket socket0 \socket ->
    Socket.setOption socket Libzmq.ZMQ_SUBSCRIBE prefix

unsubscribe :: Subscriber -> ByteString -> IO (Either Error ())
unsubscribe socket0 prefix =
  withSocket socket0 \socket ->
    Socket.setOption socket Libzmq.ZMQ_UNSUBSCRIBE prefix

receive :: Subscriber -> IO (Either Error (List.NonEmpty ByteString))
receive socket =
  withSocket socket Socket.receive
