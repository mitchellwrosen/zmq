module Zmq.API.Socket
  ( socket
  ) where

import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-socket>
socket
  :: Ptr FFI.Context
  -> FFI.Socktype
  -> IO ( Ptr FFI.Socket )
socket context socketType = do
  ptr :: Ptr FFI.Socket <-
    FFI.zmq_socket context socketType

  if ptr == nullPtr
    then FFI.zmq_errno >>= exception "zmq_socket"
    else pure ptr
