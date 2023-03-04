module Zmq.Responder
  ( Responder,
    open,
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
import Data.List.NonEmpty as List (NonEmpty)
import Libzmq
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Context (ThreadUnsafeSocket (..), withThreadUnsafeSocket)
import Zmq.Internal.Context qualified as Zmq.Internal.Socket

newtype Responder
  = Responder ThreadUnsafeSocket
  deriving stock (Eq)

open :: IO (Either Error Responder)
open =
  coerce (Zmq.Internal.Socket.openThreadUnsafeSocket ZMQ_REP)

bind :: Responder -> Endpoint transport -> IO (Either Error ())
bind (Responder socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Zmq.Internal.Socket.bind socket endpoint

unbind :: Responder -> Endpoint transport -> IO (Either Error ())
unbind (Responder socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Zmq.Internal.Socket.unbind socket endpoint

connect :: Responder -> Endpoint transport -> IO (Either Error ())
connect (Responder socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Zmq.Internal.Socket.connect socket endpoint

disconnect :: Responder -> Endpoint transport -> IO (Either Error ())
disconnect (Responder socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Zmq.Internal.Socket.disconnect socket endpoint

send :: Responder -> List.NonEmpty ByteString -> IO (Either Error ())
send (Responder socket0) message =
  withThreadUnsafeSocket socket0 \socket -> Zmq.Internal.Socket.send socket message

receive :: Responder -> IO (Either Error (List.NonEmpty ByteString))
receive (Responder socket) =
  withThreadUnsafeSocket socket Zmq.Internal.Socket.receive
