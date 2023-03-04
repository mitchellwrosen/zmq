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
import Zmq.Internal.Socket qualified as Socket

newtype Publisher
  = Publisher (MVar Zmq_socket)
  deriving stock (Eq)

open :: IO (Either Error Publisher)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_PUB)

bind :: Publisher -> Endpoint transport -> IO (Either Error ())
bind (Publisher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.bind socket endpoint

unbind :: Publisher -> Endpoint transport -> IO (Either Error ())
unbind (Publisher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.unbind socket endpoint

connect :: Publisher -> Endpoint transport -> IO (Either Error ())
connect (Publisher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.connect socket endpoint

disconnect :: Publisher -> Endpoint transport -> IO (Either Error ())
disconnect (Publisher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.disconnect socket endpoint

send :: Publisher -> ByteString -> [ByteString] -> IO (Either Error ())
send (Publisher socketVar) topic message =
  withMVar socketVar \socket -> Socket.send socket (topic List.NonEmpty.:| message)
