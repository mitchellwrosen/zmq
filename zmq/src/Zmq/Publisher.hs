module Zmq.Publisher
  ( Publisher,
    with,
    bind,
    unbind,
    connect,
    disconnect,
    send,
  )
where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import Libzmq qualified
import UnliftIO
import Zmq.Context
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Socket qualified

newtype Publisher
  = Publisher (MVar Libzmq.Zmq_socket_t)
  deriving stock (Eq)

with :: forall a. Context -> (Publisher -> IO (Either Error a)) -> IO (Either Error a)
with =
  coerce @(Context -> (MVar Libzmq.Zmq_socket_t -> IO (Either Error a)) -> IO (Either Error a)) Zmq.Internal.Socket.with

bind :: Publisher -> Endpoint transport -> IO (Either Error ())
bind =
  coerce Zmq.Internal.Socket.bind

unbind :: Publisher -> Endpoint transport -> IO (Either Error ())
unbind =
  coerce Zmq.Internal.Socket.unbind

connect :: Publisher -> Endpoint transport -> IO (Either Error ())
connect =
  coerce Zmq.Internal.Socket.connect

disconnect :: Publisher -> Endpoint transport -> IO (Either Error ())
disconnect =
  coerce Zmq.Internal.Socket.disconnect

send :: Publisher -> NonEmpty ByteString -> IO (Either Error ())
send =
  coerce Zmq.Internal.Socket.send
