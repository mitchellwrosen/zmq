module Zmq.Puller
  ( Puller,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    receive,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty as List (NonEmpty)
import Libzmq
import Zmq.Endpoint
import Zmq.Error
import Zmq.Internal.Context qualified as Zmq.Internal.Socket

newtype Puller
  = Puller (MVar Zmq_socket)
  deriving stock (Eq)

open :: IO (Either Error Puller)
open =
  coerce (Zmq.Internal.Socket.openThreadSafeSocket ZMQ_PULL)

bind :: Puller -> Endpoint transport -> IO (Either Error ())
bind (Puller socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.bind socket endpoint

unbind :: Puller -> Endpoint transport -> IO (Either Error ())
unbind (Puller socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.unbind socket endpoint

connect :: Puller -> Endpoint transport -> IO (Either Error ())
connect (Puller socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.connect socket endpoint

disconnect :: Puller -> Endpoint transport -> IO (Either Error ())
disconnect (Puller socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.disconnect socket endpoint

receive :: Puller -> IO (Either Error (List.NonEmpty ByteString))
receive (Puller socketVar) =
  withMVar socketVar Zmq.Internal.Socket.receive
