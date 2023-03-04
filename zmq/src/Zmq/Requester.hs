module Zmq.Requester
  ( Requester,
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
import Data.List.NonEmpty (NonEmpty)
import Libzmq
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Socket qualified

newtype Requester
  = Requester Zmq_socket
  deriving stock (Eq)

with :: (Requester -> IO (Either Error a)) -> IO (Either Error a)
with action =
  Zmq.Internal.Socket.with ZMQ_REQ \socket -> action (Requester socket)

bind :: Requester -> Endpoint transport -> IO (Either Error ())
bind =
  coerce Zmq.Internal.Socket.bind

unbind :: Requester -> Endpoint transport -> IO (Either Error ())
unbind =
  coerce Zmq.Internal.Socket.unbind

connect :: Requester -> Endpoint transport -> IO (Either Error ())
connect =
  coerce Zmq.Internal.Socket.connect

disconnect :: Requester -> Endpoint transport -> IO (Either Error ())
disconnect =
  coerce Zmq.Internal.Socket.disconnect

send :: Requester -> NonEmpty ByteString -> IO (Either Error ())
send =
  coerce Zmq.Internal.Socket.send

receive :: Requester -> IO (Either Error (NonEmpty ByteString))
receive =
  coerce Zmq.Internal.Socket.receive
