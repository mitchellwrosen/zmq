module Zmq.Requester
  ( Requester,
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

newtype Requester
  = Requester ThreadUnsafeSocket
  deriving stock (Eq)

open :: IO (Either Error Requester)
open =
  coerce (Zmq.Internal.Socket.openThreadUnsafeSocket ZMQ_REQ)

bind :: Requester -> Endpoint transport -> IO (Either Error ())
bind (Requester socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Zmq.Internal.Socket.bind socket endpoint

unbind :: Requester -> Endpoint transport -> IO (Either Error ())
unbind (Requester socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Zmq.Internal.Socket.unbind socket endpoint

connect :: Requester -> Endpoint transport -> IO (Either Error ())
connect (Requester socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Zmq.Internal.Socket.connect socket endpoint

disconnect :: Requester -> Endpoint transport -> IO (Either Error ())
disconnect (Requester socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Zmq.Internal.Socket.disconnect socket endpoint

send :: Requester -> List.NonEmpty ByteString -> IO (Either Error ())
send (Requester socket0) message =
  withThreadUnsafeSocket socket0 \socket -> Zmq.Internal.Socket.send socket message

receive :: Requester -> IO (Either Error (List.NonEmpty ByteString))
receive (Requester socket) =
  withThreadUnsafeSocket socket Zmq.Internal.Socket.receive
