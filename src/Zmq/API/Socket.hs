module Zmq.API.Socket
  ( socket
  , socket'
  ) where

import Zmq.Context (contextVar)
import Zmq.Error
import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-socket>
socket
  :: FFI.Socktype
  -> IO ( Maybe ( ForeignPtr FFI.Socket ) )
socket socketType = do
  context <-
    readMVar contextVar

  mask \unmask -> do
    ptr :: Ptr FFI.Socket <-
      FFI.zmq_socket context socketType

    if ptr == nullPtr
      then
        unmask do
          FFI.zmq_errno >>= \case
            EMFILE_ -> pure Nothing

            errno ->
              if errno == EFAULT_ || errno == ETERM_ then
                exception "zmq_socket" errno
              else
                unexpectedErrno "zmq_socket" errno

      else do
        foreignPtr :: ForeignPtr FFI.Socket <-
          newForeignPtr FFI.zmq_close_ptr ptr

        unmask ( pure ( Just foreignPtr ) )

-- | <http://api.zeromq.org/4-3:zmq-socket>
socket'
  :: Ptr FFI.Context
  -> FFI.Socktype
  -> IO ( Maybe ( Ptr FFI.Socket ) )
socket' context socketType = do
  ptr :: Ptr FFI.Socket <-
    FFI.zmq_socket context socketType

  if ptr == nullPtr
    then do
      FFI.zmq_errno >>= \case
        EMFILE_ -> pure Nothing

        errno ->
          if errno == EFAULT_ || errno == ETERM_ then
            exception "zmq_socket" errno
          else
            unexpectedErrno "zmq_socket" errno

    else do
      pure ( Just ptr )
