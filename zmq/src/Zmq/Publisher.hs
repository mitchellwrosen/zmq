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
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Libzmq
import UnliftIO
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Socket qualified

newtype Publisher
  = Publisher (MVar Zmq_socket)
  deriving stock (Eq)

with :: (Publisher -> IO (Either Error a)) -> IO (Either Error a)
with action =
  Zmq.Internal.Socket.with ZMQ_PUB \socket -> do
    socketVar <- newMVar socket
    action (Publisher socketVar)

bind :: Publisher -> Endpoint transport -> IO (Either Error ())
bind (Publisher socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.bind socket endpoint

unbind :: Publisher -> Endpoint transport -> IO (Either Error ())
unbind (Publisher socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.unbind socket endpoint

connect :: Publisher -> Endpoint transport -> IO (Either Error ())
connect (Publisher socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.connect socket endpoint

disconnect :: Publisher -> Endpoint transport -> IO (Either Error ())
disconnect (Publisher socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.disconnect socket endpoint

send :: Publisher -> ByteString -> List.NonEmpty ByteString -> IO (Either Error ())
send (Publisher socketVar) topic message =
  withMVar socketVar \socket -> Zmq.Internal.Socket.send socket (List.NonEmpty.cons topic message)
