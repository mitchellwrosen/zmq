module Zmq.API.Socket
  ( socket
  , pubSocket
  , subSocket
  ) where

import Zmq.Context (context)
import Zmq.Error
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-socket>
--
-- Returns 'Nothing' if the maximum number of sockets have already been opened.
-- By default, this is 1023, but can be changed with TODO.
socket
  :: forall a m.
     ( IsSocketType a
     , MonadIO m
     )
  => m ( Maybe ( Socket a ) )
socket =
  liftIO socketIO

socketIO
  :: forall a.
     IsSocketType a
  => IO ( Maybe ( Socket a ) )
socketIO =
  mask \unmask -> do
    ptr :: Ptr () <-
      FFI.zmq_socket context ( socketType @a )

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

        unmask ( pure ( Just ( coerce foreignPtr ) ) )

pubSocket
  :: MonadIO m
  => m ( Maybe ( Socket 'Pub ) )
pubSocket =
  socket

subSocket
  :: MonadIO m
  => m ( Maybe ( Socket 'Sub ) )
subSocket =
  socket
