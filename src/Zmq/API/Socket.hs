module Zmq.API.Socket
  ( socket
  , socket'
  ) where

import Zmq.Context (contextVar)
import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-socket>
socket
  :: FFI.Socktype
  -> IO ( ForeignPtr FFI.Socket )
socket socketType = do
  context <-
    readMVar contextVar

  mask_ do
    ptr :: Ptr FFI.Socket <-
      FFI.zmq_socket context socketType

    if ptr == nullPtr
      then
        FFI.zmq_errno >>= exception "zmq_socket"

      else
        newForeignPtr FFI.zmq_close_ptr ptr

-- | <http://api.zeromq.org/4-3:zmq-socket>
socket'
  :: Ptr FFI.Context
  -> FFI.Socktype
  -> IO ( Ptr FFI.Socket )
socket' context socketType = do
  ptr :: Ptr FFI.Socket <-
    FFI.zmq_socket context socketType

  if ptr == nullPtr
    then FFI.zmq_errno >>= exception "zmq_socket"
    else pure ptr
