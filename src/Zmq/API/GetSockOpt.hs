module Zmq.API.GetSockOpt
  ( getIntSockOpt
  , getSocketEventState
  , getSocketFd
  ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek, poke, sizeOf)
import System.Posix.Types (Fd(..))

import Zmq.Error
import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.FFI as FFI


getIntSockOpt
  :: Ptr FFI.Socket
  -> FFI.Sockopt
  -> IO CInt
getIntSockOpt socket option =
  alloca \int_ptr ->
    alloca \size_ptr -> do
      poke size_ptr ( fromIntegral ( sizeOf ( undefined :: CInt ) ) )

      fix \again ->
        FFI.zmq_getsockopt socket option int_ptr size_ptr >>= \case
          0 ->
            peek int_ptr

          _ ->
            FFI.zmq_errno >>= \case
              EINTR_ ->
                again

              errno ->
                exception "zmq_getsockopt" errno

getSocketEventState
  :: Ptr FFI.Socket
  -> IO CInt
getSocketEventState socket =
  getIntSockOpt socket FFI.zMQ_EVENTS

getSocketFd
  :: Ptr FFI.Socket
  -> IO Fd
getSocketFd socket =
  Fd <$> getIntSockOpt socket FFI.zMQ_FD
