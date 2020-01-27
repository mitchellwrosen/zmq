module Zmq.API.Send
  ( send
  , SendError
  ) where

import qualified Data.ByteString.Unsafe as ByteString

import Zmq.Error
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


-- |
-- @
-- data SendError
--   = 'EHOSTUNREACH'
-- @
type SendError
  = Error "send"

-- | <http://api.zeromq.org/4-3:zmq-send>
send
  :: ( CanSend typ
     , MonadIO m
     )
  => Socket typ
  -> ByteString
  -> m ( Either SendError () )
send socket message =
  liftIO ( sendIO socket message )

sendIO
  :: forall typ.
     CanSend typ
  => Socket typ
  -> ByteString
  -> IO ( Either SendError () )
sendIO socket message =
  withSocket socket \socket_ptr ->
    ByteString.unsafeUseAsCStringLen message \( ptr, len ) ->
      fix \again -> do
        FFI.zmq_send socket_ptr ptr ( fromIntegral len ) FFI.zMQ_DONTWAIT >>= \case
          -1 ->
            FFI.zmq_errno >>= \case
              EAGAIN_ -> do
                waitUntilCanSend socket_ptr ( isThreadSafe @typ )
                again

              EHOSTUNREACH_ ->
                pure ( Left EHOSTUNREACH )

              EINTR_ ->
                again

              ETERM_ ->
                errInvalidContext

              -- EFSM: "The zmq_send() operation cannot be performed on this
              --        socket at the moment due to the socket not being in the
              --        appropriate state. This error may occur with socket
              --        types that switch between several states, such as
              --        ZMQ_REP. See the messaging patterns section of
              --        zmq_socket(3) for more information.
              --
              --        This currently can't happen because I haven't added
              --        ZMQ_REP, it seems bonkers broken/useless. Need to
              --        investigate what other sockets can return EFSM.
              --
              -- ENOTSOCK: type system should prevent it
              --
              -- ENOTSUP: CanSend should prevent it

              errno ->
                bugUnexpectedErrno "zmq_send" errno


          -- Ignore number of bytes sent; why is this interesting?
          _ ->
            pure ( Right () )
