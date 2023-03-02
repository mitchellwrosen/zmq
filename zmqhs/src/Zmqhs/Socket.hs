module Zmqhs.Socket
  ( getSocketEvents,
    getSocketFd,
    setSocketSubscribe,
    setSocketUnsubscribe,
    send,
    receive,
  )
where

import Control.Concurrent (threadWaitRead)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Data.Foldable (toList)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Foreign.C (CInt, CShort, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peek, poke, sizeOf)
import Libzmq
import Libzmq.Bindings qualified as Libzmq.Bindings
import System.Posix.Types (Fd (..))
import UnliftIO
import Zmqhs.Internal.Error

-- | <http://api.zeromq.org/4-3:zmq-unbind>
--
-- May throw:
--   * @EINVAL@ if the endpoint is invalid.
--   * @ENOTSOCK@ if the socket is invalid.
--   * @ETERM@ if the context was terminated.
-- unbind :: MonadIO m => Socket -> Endpoint -> m ()
-- unbind sock endpoint = liftIO do
--   withEndpoint endpoint \endpoint' ->
--     Libzmq.zmq_unbind (unSocket sock) endpoint' >>= \case
--       0 -> pure ()
--       _ ->
--         Libzmq.errno >>= \case
--           ENOENT -> pure () -- The endpoint supplied was not previously bound.
--           errno -> throwError "zmq_unbind" errno

-- | <http://api.zeromq.org/4-3:zmq-disconnect>
--
-- May throw:
--   * @EINVAL@ if the endpoint is invalid.
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
-- disconnect :: MonadIO m => Socket -> Endpoint -> m ()
-- disconnect sock endpoint = liftIO do
--   withEndpoint endpoint \endpoint' ->
--     Libzmq.zmq_disconnect (unSocket sock) endpoint' >>= \case
--       0 -> pure ()
--       _ ->
--         Libzmq.errno >>= \case
--           ENOENT -> pure () -- The endpoint was not connected.
--           errno -> throwError "zmq_disconnect" errno
getSocketEvents :: Zmq_socket_t -> IO CInt
getSocketEvents =
  getIntSocketOption Libzmq.Bindings._ZMQ_EVENTS

getSocketFd :: Zmq_socket_t -> IO Fd
getSocketFd socket =
  Fd <$> getIntSocketOption Libzmq.Bindings._ZMQ_FD socket

getIntSocketOption :: CInt -> Zmq_socket_t -> IO CInt
getIntSocketOption option socket =
  alloca \intPtr ->
    alloca \sizePtr -> do
      poke sizePtr (fromIntegral (sizeOf (undefined :: CInt)))
      getSocketOption socket option intPtr sizePtr

getSocketOption :: Storable a => Zmq_socket_t -> CInt -> Ptr a -> Ptr CSize -> IO a
getSocketOption socket option valPtr sizePtr =
  fix \again ->
    zmq_getsockopt socket option valPtr sizePtr >>= \case
      0 -> peek valPtr
      _ ->
        zmq_errno >>= \case
          EINTR -> again
          errno -> throwError "zmq_getsockopt" errno

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @EINVAL@ if the option is invalid.
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
setSocketSubscribe :: MonadIO m => Zmq_socket_t -> ByteString -> m ()
setSocketSubscribe =
  setBinarySocketOption Libzmq.Bindings._ZMQ_SUBSCRIBE

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @EINVAL@ if the option is invalid.
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
setSocketUnsubscribe :: MonadIO m => Zmq_socket_t -> ByteString -> m ()
setSocketUnsubscribe =
  setBinarySocketOption Libzmq.Bindings._ZMQ_UNSUBSCRIBE

setBinarySocketOption :: MonadIO m => CInt -> Zmq_socket_t -> ByteString -> m ()
setBinarySocketOption option socket bytes = liftIO do
  ByteString.unsafeUseAsCStringLen bytes \(bytes', len) ->
    let set =
          zmq_setsockopt
            socket
            option
            bytes'
            (fromIntegral len)
     in fix \again ->
          set >>= \case
            0 -> pure ()
            _ ->
              zmq_errno >>= \case
                EINTR -> again
                errno -> throwError "zmq_setsockopt" errno

-- | <http://api.zeromq.org/4-3:zmq-send>
--
-- May throw:
--   * @EFSM@ if the socket is not in the appropriate state.
--   * @EHOSTUNREACH@ if the message cannot be routed.
--   * @EINVAL@ if the socket doesn't support multipart data.
--   * @ENOTSUP@ if the socket doesn't support sending.
--   * @ETERM@ if the context was terminated.
--
-- TODO only works for non-threadsafe sockets with a FD
send :: MonadIO m => Zmq_socket_t -> NonEmpty ByteString -> m ()
send socket (toList -> messages0) = liftIO do
  flip fix messages0 \loop -> \case
    [message] ->
      sendFrame socket message False
    message : messages -> do
      sendFrame socket message True
      loop messages
    [] ->
      error "send: []"

sendFrame :: Zmq_socket_t -> ByteString -> Bool -> IO ()
sendFrame socket message more =
  fix \again ->
    zmq_send_dontwait socket message more >>= \case
      Left EAGAIN -> do
        waitUntilCanSend socket
        again
      Left EINTR -> again
      Left errno -> throwError "zmq_send" errno
      Right _len -> pure () -- Ignore number of bytes sent; why is this interesting?

-- | <http://api.zeromq.org/4-3:zmq-msg-recv>
-- TODO only works for non-threadsafe sockets with a FD
-- FIXME this is busted; shouldn't reuse frame for all parts
receive :: Zmq_socket_t -> IO (NonEmpty ByteString)
receive socket =
  zmq_msg_with \frame ->
    receive_ frame socket `finally` zmq_msg_close frame

receive_ :: Zmq_msg_t -> Zmq_socket_t -> IO (NonEmpty ByteString)
receive_ frame socket =
  receiveFrame_ frame socket >>= loop []
  where
    loop :: [ByteString] -> ByteString -> IO (NonEmpty ByteString)
    loop acc1 acc0 =
      zmq_msg_more frame >>= \case
        False -> pure (acc0 :| reverse acc1)
        True -> do
          part <- receiveFrame_ frame socket
          loop (part : acc1) acc0

receiveFrame_ :: Zmq_msg_t -> Zmq_socket_t -> IO ByteString
receiveFrame_ message socket =
  fix \again ->
    zmq_msg_recv_dontwait message socket >>= \case
      Left EAGAIN -> do
        waitUntilCanReceive socket
        again
      Left EINTR -> again
      Left errno -> throwError "zmq_msg_recv" errno
      Right _len -> zmq_msg_data message

waitUntilCanReceive :: Zmq_socket_t -> IO ()
waitUntilCanReceive =
  waitUntilCan Libzmq.Bindings._ZMQ_POLLIN

waitUntilCanSend :: Zmq_socket_t -> IO ()
waitUntilCanSend =
  waitUntilCan Libzmq.Bindings._ZMQ_POLLOUT

waitUntilCan :: CShort -> Zmq_socket_t -> IO ()
waitUntilCan events socket = do
  fd <- getSocketFd socket

  fix \again -> do
    threadWaitRead fd -- "read" is not a typo
    state <- getSocketEvents socket
    -- http://api.zeromq.org/4-3:zmq-getsockopt
    --
    -- The combination of a file descriptor returned by
    -- the ZMQ_FD option being ready for reading but no
    -- actual events returned by a subsequent retrieval of
    -- the ZMQ_EVENTS option is valid; applications should
    -- simply ignore this case and restart their polling
    -- operation/event loop.
    when (state .&. fromIntegral @CShort @CInt events == 0) again
