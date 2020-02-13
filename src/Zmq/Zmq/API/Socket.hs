module Zmq.API.Socket
  ( socket
  ) where

import qualified Zmq.FFI as FFI
import qualified Libzmq

import qualified Zmqhs

import Zmq.Exception
import Zmq.Prelude


-- | <http://api.zeromq.org/4-3:zmq-socket>
socket
  :: Ptr Libzmq.Context
  -> Zmqhs.SocketType
  -> IO ( Ptr Libzmq.Socket )
socket context ( Zmqhs.SocketType socketType ) = do
  ptr :: Ptr Libzmq.Socket <-
    Libzmq.socket context socketType

  if ptr == nullPtr
    then FFI.zmq_errno >>= exception "zmq_socket"
    else pure ptr
