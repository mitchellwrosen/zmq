module Zmq.API.GetSockOpt
  ( getIntSockOpt
  , getSocketEventState
  , getSocketFd
  ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek, poke, sizeOf)
import System.Posix.Types (Fd(..))

import Zmq.Error
import Zmq.Exception (exception)
import Zmq.Prelude
import qualified Zmq.FFI as FFI


getIntSockOpt
  :: Ptr FFI.Socket
  -> FFI.Sockopt
  -> IO ( Either () CInt )
getIntSockOpt socket option =
  alloca \int_ptr ->
    alloca \size_ptr -> do
      poke size_ptr ( fromIntegral ( sizeOf ( undefined :: CInt ) ) )

      fix \again ->
        FFI.zmq_getsockopt socket option int_ptr size_ptr >>= \case
          0 ->
            Right <$> peek int_ptr

          _ ->
            FFI.zmq_errno >>= \case
              EINTR_ -> again
              EINVAL_ -> pure ( Left () )
              errno ->
                if errno == ETERM_ then
                  exception "zmq_getsockopt" errno
                else
                  bugUnexpectedErrno "zmq_getsockopt" errno

getSocketEventState
  :: Ptr FFI.Socket
  -> IO CInt
getSocketEventState socket =
  getIntSockOpt socket FFI.zMQ_EVENTS >>= \case
    Left () ->
      -- Don't see how ZMQ_FD could return EINVAL
      bugUnexpectedErrno "zmq_getsockopt ZMQ_EVENTS" EINVAL_

    Right n ->
      pure n

getSocketFd
  :: Ptr FFI.Socket
  -> IO Fd
getSocketFd socket =
  getIntSockOpt socket FFI.zMQ_FD >>= \case
    -- IsThreadSafe and using the right size buffers should prevent EINVAL
    Left () ->
      bugUnexpectedErrno "zmq_getsockopt ZMQ_FD" EINVAL_

    Right fd ->
      pure ( Fd fd )
