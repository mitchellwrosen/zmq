module Zmq.Pusher
  ( Pusher,
    open,
    send,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as List (NonEmpty)
import Libzmq
import Zmq.Error (Error)
import Zmq.Internal.Socket (Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket

newtype Pusher
  = Pusher (MVar Zmq_socket)
  deriving stock (Eq)

instance Socket Pusher where
  withSocket (Pusher socketVar) =
    withMVar socketVar

open :: IO (Either Error Pusher)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_PUSH)

send :: Pusher -> List.NonEmpty ByteString -> IO (Either Error ())
send socket0 message =
  withSocket socket0 \socket -> Socket.send socket message
