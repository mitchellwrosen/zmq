module Zmq.Responder
  ( Responder,
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

newtype Responder
  = Responder ThreadUnsafeSocket
  deriving stock (Eq)
  deriving newtype (Socket)

open :: IO (Either Error Responder)
open =
  coerce (Socket.openThreadUnsafeSocket ZMQ_REP)

send :: Responder -> List.NonEmpty ByteString -> IO (Either Error ())
send socket0 message =
  withSocket socket0 \socket -> Socket.send socket message

receive :: Responder -> IO (Either Error (List.NonEmpty ByteString))
receive socket =
  withSocket socket Socket.receive
