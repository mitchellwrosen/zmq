module Zmq.API.Send
  ( send
  , SendError
  ) where

import Data.Bits (testBit)
import GHC.Conc (threadWaitRead)
import qualified Data.ByteString.Unsafe as ByteString

import Zmq.API.GetSockOpt (getSocketEventState, getSocketFd)
import Zmq.Error
import Zmq.Function
import Zmq.Prelude
import Zmq.Socket
import Zmq.Util.SBool (SBool(..))
import qualified Zmq.FFI as FFI


type SendError
  = Error 'Function'Send

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
  withSocket socket \c_socket ->
    ByteString.unsafeUseAsCStringLen message \( ptr, len ) ->
      fix \sendAgain -> do
        putStrLn "zmq_send"
        FFI.zmq_send c_socket ptr ( fromIntegral len ) FFI.zMQ_DONTWAIT >>= \case
          -1 -> do
            putStrLn "done"
            FFI.zmq_errno >>= \case
              EAGAIN_ -> do
                putStrLn "EAGAIN"
                case threadSafeEvidence @typ of
                  SFalse -> do
                    putStrLn "getSocketFd"
                    fd <- getSocketFd socket
                    fix \waitAgain -> do
                      putStrLn "threadWaitRead"
                      threadWaitRead fd -- "read" is not a typo
                      putStrLn "getSocketEventState"
                      state <- getSocketEventState socket
                      if testBit state ( fromIntegral FFI.zMQ_POLLOUT )
                        then
                          sendAgain
                        else
                          -- http://api.zeromq.org/4-3:zmq-getsockopt
                          --
                          -- The combination of a file descriptor returned by
                          -- the ZMQ_FD option being ready for reading but no
                          -- actual events returned by a subsequent retrieval of
                          -- the ZMQ_EVENTS option is valid; applications should
                          -- simply ignore this case and restart their polling
                          -- operation/event loop.
                          waitAgain

                  STrue ->
                    bugIO "handling EAGAIN on thread-safe sockets is not implemented"

              EHOSTUNREACH_ ->
                pure ( Left EHOSTUNREACH )

              EINTR_ -> do
                putStrLn "EINTR"
                sendAgain

              ETERM_ ->
                errInvalidContext

              -- EINVAL: "The sender tried to send multipart data, which the
              --          socket type does not allow."
              --
              --         This currently can't happen because send only sends
              --         a single message part; will have to revisit this soon
              --
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
