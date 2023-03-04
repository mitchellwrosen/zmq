module Zmq.Pusher
  ( Pusher,
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
import Data.List.NonEmpty qualified as List (NonEmpty)
import Libzmq
import UnliftIO
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Socket qualified as Socket

newtype Pusher
  = Pusher (MVar Zmq_socket)
  deriving stock (Eq)

open :: IO (Either Error Pusher)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_PUSH)

bind :: Pusher -> Endpoint transport -> IO (Either Error ())
bind (Pusher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.bind socket endpoint

unbind :: Pusher -> Endpoint transport -> IO (Either Error ())
unbind (Pusher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.unbind socket endpoint

connect :: Pusher -> Endpoint transport -> IO (Either Error ())
connect (Pusher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.connect socket endpoint

disconnect :: Pusher -> Endpoint transport -> IO (Either Error ())
disconnect (Pusher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.disconnect socket endpoint

send :: Pusher -> List.NonEmpty ByteString -> IO (Either Error ())
send (Pusher socketVar) message =
  withMVar socketVar \socket -> Socket.send socket message
