module Zmq.API.Send
  ( nonBlockingSend
  , nonThreadsafeSend
  , SendError
  ) where

import Data.Bits ((.|.))
import qualified Data.ByteString.Unsafe as ByteString

import Zmq.Error
import Zmq.Exception
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

-- | Send on a socket that won't block (like PUB).
nonBlockingSend
  :: Ptr FFI.Socket
  -> NonEmpty ByteString
  -> IO ( Either CInt () )
nonBlockingSend socket =
  nonBlockingSend_ socket . toList

nonBlockingSend_
  :: Ptr FFI.Socket
  -> [ ByteString ] -- Invariant: non-empty
  -> IO ( Either CInt () )
nonBlockingSend_ socket =
  fix \loop -> \case
    [ message ] ->
      nonBlockingSend__ socket message 0

    message : messages ->
      nonBlockingSend__ socket message FFI.zMQ_SNDMORE >>= \case
        Left errno -> pure ( Left errno )
        Right () -> loop messages

    [] ->
      error "nonBlockingSend_: []"

nonBlockingSend__
  :: Ptr FFI.Socket
  -> ByteString
  -> CInt
  -> IO ( Either CInt () )
nonBlockingSend__ socket message flags =
  ByteString.unsafeUseAsCStringLen message \( ptr, fromIntegral -> len ) ->
    fix \again ->
      FFI.zmq_send socket ptr len ( FFI.zMQ_DONTWAIT .|. flags ) >>= \case
        -1 ->
          FFI.zmq_errno >>= \case
            EINTR_ ->
              again

            errno ->
              if errno == ETERM_ then
                exception "zmq_send" errno
              else
                pure ( Left errno )

        -- Ignore number of bytes sent; why is this interesting?
        _ ->
          pure ( Right () )

-- | Send on a socket that won't block, but isn't threadsafe (i.e. getting its
-- file descriptor works).
nonThreadsafeSend
  :: ForeignPtr FFI.Socket
  -> ByteString
  -> IO ( Either SendError () )
nonThreadsafeSend socket message =
  withForeignPtr socket \socket_ptr ->
    ByteString.unsafeUseAsCStringLen message \( ptr, len ) ->
      fix \again -> do
        FFI.zmq_send socket_ptr ptr ( fromIntegral len ) FFI.zMQ_DONTWAIT >>= \case
          -1 ->
            FFI.zmq_errno >>= \case
              EAGAIN_ -> do
                nonThreadsafeWaitUntilCanSend socket_ptr
                again

              EHOSTUNREACH_ ->
                pure ( Left EHOSTUNREACH )

              EINTR_ ->
                again

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
                if errno == ETERM_ then
                  exception "zmq_send" errno
                else
                  unexpectedErrno "zmq_send" errno


          -- Ignore number of bytes sent; why is this interesting?
          _ ->
            pure ( Right () )
