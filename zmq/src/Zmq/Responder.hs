module Zmq.Responder
  ( Responder,
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

newtype Responder
  = Responder Zmq_socket_t
  deriving stock (Eq)

with :: (Responder -> IO (Either Error a)) -> IO (Either Error a)
with action =
  Zmq.Internal.Socket.with ZMQ_REP \socket -> action (Responder socket)

bind :: Responder -> Endpoint transport -> IO (Either Error ())
bind =
  coerce Zmq.Internal.Socket.bind

unbind :: Responder -> Endpoint transport -> IO (Either Error ())
unbind =
  coerce Zmq.Internal.Socket.unbind

connect :: Responder -> Endpoint transport -> IO (Either Error ())
connect =
  coerce Zmq.Internal.Socket.connect

disconnect :: Responder -> Endpoint transport -> IO (Either Error ())
disconnect =
  coerce Zmq.Internal.Socket.disconnect

send :: Responder -> NonEmpty ByteString -> IO (Either Error ())
send =
  coerce Zmq.Internal.Socket.send

receive :: Responder -> IO (Either Error (NonEmpty ByteString))
receive =
  coerce Zmq.Internal.Socket.receive
