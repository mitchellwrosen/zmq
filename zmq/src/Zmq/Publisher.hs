module Zmq.Publisher
  ( Publisher,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    send,
  )
where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as List.NonEmpty
import Libzmq
import UnliftIO
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Context qualified as Zmq.Internal.Socket

newtype Publisher
  = Publisher (MVar Zmq_socket)
  deriving stock (Eq)

open :: IO (Either Error Publisher)
open =
  coerce (Zmq.Internal.Socket.openThreadSafeSocket ZMQ_PUB)

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

send :: Publisher -> ByteString -> [ByteString] -> IO (Either Error ())
send (Publisher socketVar) topic message =
  withMVar socketVar \socket -> Zmq.Internal.Socket.send socket (topic List.NonEmpty.:| message)
