module Zmq.API.Recv
  ( nonThreadsafeRecv
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Foreign.Marshal.Alloc (alloca)
import qualified Data.ByteString as ByteString

import qualified Libzmq
import qualified Zmq.FFI as FFI

import qualified Zmqhs

import Zmq.Error
import Zmq.Exception
import Zmq.Prelude
import Zmq.Socket


-- | <http://api.zeromq.org/4-3:zmq-msg-recv>
nonThreadsafeRecv
  :: Zmqhs.Socket
  -> IO ( NonEmpty ByteString )
nonThreadsafeRecv socket =
  withFrame \frame_ptr ->
    nonThreadsafeRecv_ frame_ptr socket

nonThreadsafeRecv_
  :: Ptr Libzmq.Frame
  -> Zmqhs.Socket
  -> IO ( NonEmpty ByteString )
nonThreadsafeRecv_ frame socket =
  recv1 >>= loop []

  where
    recv1 :: IO ByteString
    recv1 =
      fix \again ->
        Libzmq.receiveFrame frame ( Zmqhs.unSocket socket ) FFI.zMQ_DONTWAIT >>= \case
          -1 ->
            Libzmq.errno >>= \case
              EAGAIN -> do
                nonThreadsafeWaitUntilCanRecv socket
                again

              EINTR ->
                again

              errno ->
                exception "zmq_msg_recv" errno

          len -> do
            data_ptr <- Libzmq.getFrameData frame
            ByteString.packCStringLen ( data_ptr, fromIntegral len )

    loop
      :: [ ByteString ]
      -> ByteString
      -> IO ( NonEmpty ByteString )
    loop acc1 acc0 =
      Libzmq.getFrameProperty frame FFI.zMQ_MORE >>= \case
        1 -> do
          part <- recv1
          loop ( part : acc1 ) acc0

        _ ->
          pure ( acc0 :| reverse acc1 )

withFrame
  :: ( Ptr Libzmq.Frame -> IO a )
  -> IO a
withFrame f =
  alloca \frame ->
    bracket_
      ( Libzmq.initializeFrame frame )
      ( Libzmq.closeFrame frame )
      ( f frame )
