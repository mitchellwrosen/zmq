module Zmq.Requester
  ( Requester,
    open,
    send,
    receive,
  )
where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty as List (NonEmpty)
import Libzmq
import Zmq.Error (Error)
import Zmq.Internal.Socket (Socket (withSocket), ThreadUnsafeSocket (..))
import Zmq.Internal.Socket qualified as Socket

newtype Requester
  = Requester ThreadUnsafeSocket
  deriving stock (Eq)
  deriving newtype (Socket)

open :: IO (Either Error Requester)
open =
  coerce (Socket.openThreadUnsafeSocket ZMQ_REQ)

send :: Requester -> List.NonEmpty ByteString -> IO (Either Error ())
send socket0 message =
  withSocket socket0 \socket -> Socket.send socket message

receive :: Requester -> IO (Either Error (List.NonEmpty ByteString))
receive socket =
  withSocket socket Socket.receive
