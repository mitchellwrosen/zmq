module Zmq.API.Recv
  ( nonThreadsafeRecv
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Foreign.Marshal.Alloc (alloca)
import qualified Data.ByteString as ByteString

import Zmq.Error
import Zmq.Exception
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-msg-recv>
nonThreadsafeRecv
  :: ForeignPtr FFI.Socket
  -> IO ( NonEmpty ByteString )
nonThreadsafeRecv socket =
  withFrame \frame_ptr ->
    withForeignPtr socket \socket_ptr ->
      nonThreadsafeRecv_ frame_ptr socket_ptr

nonThreadsafeRecv_
  :: Ptr FFI.Frame
  -> Ptr FFI.Socket
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
              EAGAIN_ -> do
                nonThreadsafeWaitUntilCanRecv socket
                again

              EINTR_ ->
                again

              -- EFSM: "The zmq_msg_recv() operation cannot be performed on
              --        this socket at the moment due to the socket not
              --        being in the appropriate state. This error may occur
              --        with socket types that switch between several
              --        states, such as ZMQ_REP. See the messaging patterns
              --        section of zmq_socket(3) for more information.
              --
              --        This currently can't happen because I haven't added
              --        ZMQ_REP, it seems bonkers broken/useless. Need to
              --        investigate what other sockets can return EFSM.

              errno ->
                if errno == ETERM_ then
                  exception "zmq_msg_recv" errno
                else
                  unexpectedErrno "zmq_msg_recv" errno

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
