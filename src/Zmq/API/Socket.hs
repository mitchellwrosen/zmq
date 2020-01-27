module Zmq.API.Socket
  ( socket
  ) where

import Zmq.Context (context)
import Zmq.Error
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-socket>
--
-- Returns 'Nothing' if the maximum number of sockets have already been opened.
-- By default, this is 1023, but can be changed with TODO.
socket
  :: CInt
  -> IO ( Maybe ( ForeignPtr FFI.Socket ) )
socket socketType =
  mask \unmask -> do
    ptr :: Ptr () <-
      FFI.zmq_socket context socketType

    if ptr == nullPtr
      then
        unmask do
          FFI.zmq_errno >>= \case
            EMFILE_ -> pure Nothing

            EFAULT_ -> errInvalidContext
            ETERM_  -> errInvalidContext

            -- EINVAL: type system should prevent it

            errno ->
              bugUnexpectedErrno "zmq_socket" errno

      else do
        foreignPtr :: ForeignPtr () <-
          newForeignPtr FFI.zmq_close ptr

        unmask ( pure ( Just foreignPtr ) )
