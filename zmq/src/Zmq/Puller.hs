module Zmq.Puller
  ( Puller,
    open,
    receive,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty as List (NonEmpty)
import Libzmq
import Zmq.Error
import Zmq.Internal.Socket (Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket

newtype Puller
  = Puller (MVar Zmq_socket)
  deriving stock (Eq)

instance Socket Puller where
  withSocket (Puller socketVar) =
    withMVar socketVar

open :: IO (Either Error Puller)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_PULL)

receive :: Puller -> IO (Either Error (List.NonEmpty ByteString))
receive socket =
  withSocket socket Socket.receive
