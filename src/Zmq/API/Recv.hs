module Zmq.API.Recv
  ( recv
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import Foreign.Marshal.Alloc (alloca)
import qualified Data.ByteString as ByteString

import Zmq.Error
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-msg-recv>
recv
  :: ( CanReceive typ
     , MonadIO m
     )
  => Socket typ
  -> m ( NonEmpty ByteString )
recv socket =
  liftIO ( recvIO socket )

recvIO
  :: forall typ.
     IsSocketType typ
  => Socket typ
  -> IO ( NonEmpty ByteString )
recvIO socket =
  withMessagePart \message_ptr ->
    withSocket socket \socket_ptr ->
      let
        recv1 :: IO ByteString
        recv1 =
          fix \again ->
            FFI.zmq_msg_recv message_ptr socket_ptr FFI.zMQ_DONTWAIT >>= \case
              -1 ->
                FFI.zmq_errno >>= \case
                  EAGAIN_ -> do
                    waitUntilCanRecv socket_ptr ( isThreadSafe @typ )
                    again

                  EINTR_ ->
                    again

                  ETERM_ ->
                    errInvalidContext

                  -- EFAULT: "The message passed to the function was invalid."
                  --
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
                  --
                  -- ENOTSOCK: type system should prevent it
                  --
                  -- ENOTSUP: CanReceive should prevent it

                  errno ->
                    bugUnexpectedErrno "zmq_msg_recv" errno

              len -> do
                data_ptr <- FFI.zmq_msg_data message_ptr
                ByteString.packCStringLen ( data_ptr, fromIntegral len )

        loop
          :: ByteString
          -> [ ByteString ]
          -> IO ( NonEmpty ByteString )
        loop acc0 acc1 =
          FFI.zmq_msg_get message_ptr FFI.zMQ_MORE >>= \case
            1 -> do
              part <- recv1
              loop acc0 ( part : acc1 )

            _ ->
              pure ( acc0 :| reverse acc1 )
      in do
        part <- recv1
        loop part []

withMessagePart
  :: ( Ptr FFI.Message -> IO a )
  -> IO a
withMessagePart f =
  alloca \msg_ptr ->
    bracket_
      ( FFI.zmq_msg_init msg_ptr )
      ( FFI.zmq_msg_close msg_ptr )
      ( f msg_ptr )
