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
import Zmq.Internal.Socket (ThreadUnsafeSocket (..), withThreadUnsafeSocket)
import Zmq.Internal.Socket qualified as Socket

newtype Requester
  = Requester ThreadUnsafeSocket
  deriving stock (Eq)

open :: IO (Either Error Requester)
open =
  coerce (Socket.openThreadUnsafeSocket ZMQ_REQ)

bind :: Requester -> Endpoint transport -> IO (Either Error ())
bind (Requester socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Socket.bind socket endpoint

unbind :: Requester -> Endpoint transport -> IO (Either Error ())
unbind (Requester socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Socket.unbind socket endpoint

connect :: Requester -> Endpoint transport -> IO (Either Error ())
connect (Requester socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Socket.connect socket endpoint

disconnect :: Requester -> Endpoint transport -> IO (Either Error ())
disconnect (Requester socket0) endpoint =
  withThreadUnsafeSocket socket0 \socket -> Socket.disconnect socket endpoint

send :: Requester -> List.NonEmpty ByteString -> IO (Either Error ())
send (Requester socket0) message =
  withThreadUnsafeSocket socket0 \socket -> Socket.send socket message

receive :: Requester -> IO (Either Error (List.NonEmpty ByteString))
receive (Requester socket) =
  withThreadUnsafeSocket socket Socket.receive
