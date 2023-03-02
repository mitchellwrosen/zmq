module Zmq.Internal.Socket
  ( with,
    setByteStringOption,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    receive,
  )
where

import Control.Concurrent (threadWaitRead)
import Control.Concurrent.MVar
import Control.Exception
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.Functor (void, (<&>))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Foreign.C.Types (CInt, CShort)
import Libzmq
import Libzmq.Bindings qualified
import System.Posix.Types (Fd (..))
import Zmq.Context
import Zmq.Endpoint
import Zmq.Error
import Zmq.Internal (renderEndpoint)

with :: Context -> (MVar Zmq_socket_t -> IO (Either Error a)) -> IO (Either Error a)
with context action =
  mask \restore ->
    open context >>= \case
      Left err -> pure (Left err)
      Right socket -> do
        socketVar <- newMVar socket
        try (restore (action socketVar)) >>= \case
          Left (exception :: SomeException) -> do
            uninterruptibleMask_ (void (close socket))
            throwIO exception
          Right result -> do
            uninterruptibleMask_ (close socket) <&> \case
              Left err ->
                Left case result of
                  Left err0 -> err0 -- prefer user's error to close error
                  Right _ -> err
              Right () -> result

open :: Context -> IO (Either Error Zmq_socket_t)
open (Context context) =
  zmq_socket context ZMQ_SUB <&> \case
    Left err -> Left (enrichError "zmq_socket" err)
    Right socket -> Right socket

close :: Zmq_socket_t -> IO (Either Error ())
close socket =
  zmq_close socket <&> \case
    Left err -> Left (enrichError "zmq_close" err)
    Right () -> Right ()

setByteStringOption :: Zmq_socket_t -> CInt -> ByteString -> IO (Either Error ())
setByteStringOption socket option value =
  zmq_setsockopt_bytestring socket option value >>= \case
    Left errno ->
      undefined
        let badCall = throwIO err
            err = enrichError "zmq_setsockopt" errno
         in case errno of
              EINTR -> setByteStringOption socket option value
              EINVAL -> badCall
              ENOTSOCK -> badCall
              ETERM -> pure (Left err)
              _ -> unexpectedError err
    Right val -> pure (Right val)

getIntOption :: Zmq_socket_t -> CInt -> IO (Either Error Int)
getIntOption socket option =
  zmq_getsockopt_int socket option >>= \case
    Left errno ->
      let badCall = throwIO err
          err = enrichError "zmq_getsockopt" errno
       in case errno of
            EINTR -> getIntOption socket option
            EINVAL -> badCall
            ENOTSOCK -> badCall
            ETERM -> pure (Left err)
            _ -> unexpectedError err
    Right val -> pure (Right val)

bind :: MVar Zmq_socket_t -> Endpoint transport -> IO (Either Error ())
bind socketVar endpoint =
  withMVar socketVar \socket ->
    enrichFunction "zmq_bind" (zmq_bind socket (renderEndpoint endpoint))

unbind :: MVar Zmq_socket_t -> Endpoint transport -> IO (Either Error ())
unbind socketVar endpoint =
  withMVar socketVar \socket ->
    zmq_unbind socket (renderEndpoint endpoint) >>= \case
      Left errno ->
        let badCall = throwIO err
            err = enrichError "zmq_unbind" errno
         in case errno of
              EINVAL -> badCall
              ENOENT -> pure (Right ()) -- silence these
              ENOTSOCK -> badCall
              ETERM -> pure (Left err)
              _ -> unexpectedError err
      Right () -> pure (Right ())

connect :: MVar Zmq_socket_t -> Endpoint transport -> IO (Either Error ())
connect socketVar endpoint =
  withMVar socketVar \socket ->
    enrichFunction "zmq_connect" (zmq_connect socket (renderEndpoint endpoint))

disconnect :: MVar Zmq_socket_t -> Endpoint transport -> IO (Either Error ())
disconnect socketVar endpoint =
  withMVar socketVar \socket ->
    enrichFunction "zmq_disconnect" (zmq_disconnect socket (renderEndpoint endpoint))

send :: MVar Zmq_socket_t -> NonEmpty ByteString -> IO (Either Error ())
send socketVar message =
  withMVar socketVar \socket ->
    let loop = \case
          [frame] -> send1 socket frame False
          frame : frames ->
            send1 socket frame True >>= \case
              Left err -> pure (Left err)
              Right () -> loop frames
          [] -> undefined -- impossible
     in loop (List.NonEmpty.toList message)

send1 :: Zmq_socket_t -> ByteString -> Bool -> IO (Either Error ())
send1 socket frame more =
  zmq_send_dontwait socket frame more >>= \case
    Left errno ->
      let badCall = throwIO err
          err = enrichError "zmq_send" errno
       in case errno of
            EAGAIN ->
              getIntOption socket Libzmq.Bindings._ZMQ_FD >>= \case
                Left err1 -> pure (Left err1)
                Right fd -> do
                  let loop = do
                        threadWaitRead (fromIntegral @Int @Fd fd) -- "read" is not a typo
                        getIntOption socket Libzmq.Bindings._ZMQ_EVENTS >>= \case
                          Left err1 -> pure (Left err1)
                          Right events ->
                            if events .&. fromIntegral @CShort @Int Libzmq.Bindings._ZMQ_POLLOUT == 0
                              then loop
                              else send1 socket frame more
                  loop
            EFSM -> badCall
            EHOSTUNREACH -> pure (Left err)
            EINVAL -> badCall
            EINTR -> send1 socket frame more
            ENOTSUP -> badCall
            ENOTSOCK -> badCall
            ETERM -> pure (Left err)
            _ -> unexpectedError err
    Right _len -> pure (Right ())

receive :: MVar Zmq_socket_t -> IO (NonEmpty ByteString)
receive socketVar =
  withMVar socketVar undefined -- Zmqhs.receive
