module Zmq.API.Recv
  ( nonThreadsafeRecv
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Foreign.Marshal.Alloc (alloca)
import qualified Data.ByteString as ByteString

import Zmq.Error
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-msg-recv>
nonThreadsafeRecv
  :: ForeignPtr FFI.Socket
  -> IO ( NonEmpty ByteString )
nonThreadsafeRecv socket =
  withMessagePart \message_ptr ->
    withForeignPtr socket \socket_ptr ->
      nonThreadsafeRecv_ message_ptr socket_ptr

nonThreadsafeRecv_
  :: Ptr FFI.Message
  -> Ptr FFI.Socket
  -> IO ( NonEmpty ByteString )
nonThreadsafeRecv_ message socket =
  recv1 >>= loop []

  where
    recv1 :: IO ByteString
    recv1 =
      fix \again ->
        FFI.zmq_msg_recv message socket FFI.zMQ_DONTWAIT >>= \case
          -1 ->
            FFI.zmq_errno >>= \case
              EAGAIN_ -> do
                waitUntilCanRecv socket False
                again

              EINTR_ ->
                again

              ETERM_ ->
                errInvalidContext

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
                bugUnexpectedErrno "zmq_msg_recv" errno

          len -> do
            data_ptr <- FFI.zmq_msg_data message
            ByteString.packCStringLen ( data_ptr, fromIntegral len )

    loop
      :: [ ByteString ]
      -> ByteString
      -> IO ( NonEmpty ByteString )
    loop acc1 acc0 =
      FFI.zmq_msg_get message FFI.zMQ_MORE >>= \case
        1 -> do
          part <- recv1
          loop ( part : acc1 ) acc0

        _ ->
          pure ( acc0 :| reverse acc1 )

withMessagePart
  :: ( Ptr FFI.Message -> IO a )
  -> IO a
withMessagePart f =
  alloca \msg_ptr ->
    bracket_
      ( FFI.zmq_msg_init msg_ptr )
      ( FFI.zmq_msg_close msg_ptr )
      ( f msg_ptr )
