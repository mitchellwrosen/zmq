module Zmq.API.Recv
  ( nonThreadsafeRecv
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Foreign.Marshal.Alloc (alloca)
import qualified Data.ByteString as ByteString

import qualified Libzmq
import qualified Zmq.FFI as FFI

import Zmq.Error
import Zmq.Exception
import Zmq.Prelude
import Zmq.Socket


-- | <http://api.zeromq.org/4-3:zmq-msg-recv>
nonThreadsafeRecv
  :: Ptr Libzmq.Socket
  -> IO ( NonEmpty ByteString )
nonThreadsafeRecv socket =
  withFrame \frame_ptr ->
    nonThreadsafeRecv_ frame_ptr socket

nonThreadsafeRecv_
  :: Ptr FFI.Frame
  -> Ptr Libzmq.Socket
  -> IO ( NonEmpty ByteString )
nonThreadsafeRecv_ frame socket =
  recv1 >>= loop []

  where
    recv1 :: IO ByteString
    recv1 =
      fix \again ->
        FFI.zmq_msg_recv frame socket FFI.zMQ_DONTWAIT >>= \case
          -1 ->
            FFI.zmq_errno >>= \case
              EAGAIN -> do
                nonThreadsafeWaitUntilCanRecv socket
                again

              EINTR ->
                again

              errno ->
                exception "zmq_msg_recv" errno

          len -> do
            data_ptr <- FFI.zmq_msg_data frame
            ByteString.packCStringLen ( data_ptr, fromIntegral len )

    loop
      :: [ ByteString ]
      -> ByteString
      -> IO ( NonEmpty ByteString )
    loop acc1 acc0 =
      FFI.zmq_msg_get frame FFI.zMQ_MORE >>= \case
        1 -> do
          part <- recv1
          loop ( part : acc1 ) acc0

        _ ->
          pure ( acc0 :| reverse acc1 )

withFrame
  :: ( Ptr FFI.Frame -> IO a )
  -> IO a
withFrame f =
  alloca \frame ->
    bracket_
      ( FFI.zmq_msg_init frame )
      ( FFI.zmq_msg_close frame )
      ( f frame )
