module Zmqhs.Socket
  ( Socket (..),
    open,
    close,
    with,
    bind,
    unbind,
    connect,
    disconnect,
    getSocketEvents,
    getSocketFd,
    setSocketSubscribe,
    setSocketUnsubscribe,
    send,
    receive,
  )
where

import Control.Concurrent (threadWaitRead)
import Control.Monad (when)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString
import Data.Foldable (toList)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Foreign.C (CInt, CSize)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable, peek, poke, sizeOf)
import Libzmq qualified
import System.Posix.Types (Fd (..))
import UnliftIO
import Zmqhs.Context (Context (..))
import Zmqhs.Endpoint (Endpoint, withEndpoint)
import Zmqhs.Frame (Frame (..), copyFrameBytes, isLastFrame, withTemporaryFrame)
import Zmqhs.Internal.Error
import Zmqhs.SocketType (SocketType (..))

newtype Socket = Socket
  {unSocket :: Ptr Libzmq.Socket}
  deriving stock (Eq, Ord, Show)

-- | <http://api.zeromq.org/4-3:zmq-socket>
--
-- May throw:
--   * @EMFILE@ if no more sockets can be opened.
--   * @ETERM@ if the context was terminated.
open :: MonadIO m => Context -> SocketType -> m Socket
open context socketType = liftIO do
  sock <- Libzmq.socket (unContext context) (socketTypeToCInt socketType)
  if sock /= nullPtr
    then pure (Socket sock)
    else Libzmq.errno >>= throwError "zmq_socket"
  where
    socketTypeToCInt :: SocketType -> CInt
    socketTypeToCInt = \case
      Pub -> Libzmq.zMQ_PUB
      Sub -> Libzmq.zMQ_SUB
      XPub -> Libzmq.zMQ_XPUB
      XSub -> Libzmq.zMQ_XSUB

-- | <http://api.zeromq.org/4-3:zmq-close>
close :: MonadIO m => Socket -> m ()
close sock = liftIO do
  Libzmq.close (unSocket sock)

with :: MonadUnliftIO m => Context -> SocketType -> (Socket -> m a) -> m a
with context socketType =
  bracket (open context socketType) close

-- | <http://api.zeromq.org/4-3:zmq-bind>
--
-- May throw:
--   * @EADDRINUSE@ if the address is already in use.
--   * @EADDRNOTAVAIL@ if the address is not local.
--   * @EINVAL@ if the endpoint is invalid.
--   * @EMTHREAD@ if no IO thread is available.
--   * @ENOCOMPATPROTO@ if the protocol is not compatible with the socket type.
--   * @ENODEV@ if the address specifies a nonexistent interface.
--   * @ENOTSOCK@ if the socket is invalid.
--   * @EPROTONOSUPPORT@ if the protocol is not supported.
--   * @ETERM@ if the context was terminated.
bind :: MonadIO m => Socket -> Endpoint -> m ()
bind sock endpoint = liftIO do
  withEndpoint endpoint \endpoint' ->
    Libzmq.bind (unSocket sock) endpoint' >>= \case
      0 -> pure ()
      _ -> Libzmq.errno >>= throwError "zmq_bind"

-- | <http://api.zeromq.org/4-3:zmq-unbind>
--
-- May throw:
--   * @EINVAL@ if the endpoint is invalid.
--   * @ENOTSOCK@ if the socket is invalid.
--   * @ETERM@ if the context was terminated.
unbind :: MonadIO m => Socket -> Endpoint -> m ()
unbind sock endpoint = liftIO do
  withEndpoint endpoint \endpoint' ->
    Libzmq.unbind (unSocket sock) endpoint' >>= \case
      0 -> pure ()
      _ ->
        Libzmq.errno >>= \case
          ENOENT -> pure () -- The endpoint supplied was not previously bound.
          errno -> throwError "zmq_unbind" errno

-- | <http://api.zeromq.org/4-3:zmq-connect>
--
-- May throw:
--   * @EINVAL@ if the endpoint is invalid.
--   * @EMTHREAD@ if no IO thread is available.
--   * @ENOCOMPATPROTO@ if the protocol is not compatible with the socket type.
--   * @ENOTSOCK@ if the socket is invalid.
--   * @EPROTONOSUPPORT@ if the protocol is not supported.
--   * @ETERM@ if the context was terminated.
connect :: MonadIO m => Socket -> Endpoint -> m ()
connect sock endpoint = liftIO do
  withEndpoint endpoint \endpoint' ->
    Libzmq.connect (unSocket sock) endpoint' >>= \case
      0 -> pure ()
      _ -> Libzmq.errno >>= throwError "zmq_connect"

-- | <http://api.zeromq.org/4-3:zmq-disconnect>
--
-- May throw:
--   * @EINVAL@ if the endpoint is invalid.
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
disconnect :: MonadIO m => Socket -> Endpoint -> m ()
disconnect sock endpoint = liftIO do
  withEndpoint endpoint \endpoint' ->
    Libzmq.disconnect (unSocket sock) endpoint' >>= \case
      0 -> pure ()
      _ ->
        Libzmq.errno >>= \case
          ENOENT -> pure () -- The endpoint was not connected.
          errno -> throwError "zmq_disconnect" errno

getSocketEvents :: Socket -> IO CInt
getSocketEvents =
  getIntSocketOption Libzmq.zMQ_EVENTS

getSocketFd :: Socket -> IO Fd
getSocketFd socket =
  Fd <$> getIntSocketOption Libzmq.zMQ_FD socket

getIntSocketOption :: CInt -> Socket -> IO CInt
getIntSocketOption option socket =
  alloca \intPtr ->
    alloca \sizePtr -> do
      poke sizePtr (fromIntegral (sizeOf (undefined :: CInt)))
      getSocketOption socket option intPtr sizePtr

getSocketOption :: Storable a => Socket -> CInt -> Ptr a -> Ptr CSize -> IO a
getSocketOption socket option valPtr sizePtr =
  fix \again ->
    Libzmq.getSocketOption (unSocket socket) option valPtr sizePtr >>= \case
      0 -> peek valPtr
      _ ->
        Libzmq.errno >>= \case
          EINTR -> again
          errno -> throwError "zmq_getsockopt" errno

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @EINVAL@ if the option is invalid.
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
setSocketSubscribe :: MonadIO m => Socket -> ByteString -> m ()
setSocketSubscribe =
  setBinarySocketOption Libzmq.zMQ_SUBSCRIBE

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @EINVAL@ if the option is invalid.
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
setSocketUnsubscribe :: MonadIO m => Socket -> ByteString -> m ()
setSocketUnsubscribe =
  setBinarySocketOption Libzmq.zMQ_UNSUBSCRIBE

setBinarySocketOption :: MonadIO m => CInt -> Socket -> ByteString -> m ()
setBinarySocketOption option sock bytes = liftIO do
  ByteString.unsafeUseAsCStringLen bytes \(bytes', len) ->
    let set =
          Libzmq.setSocketOption
            (unSocket sock)
            option
            bytes'
            (fromIntegral len)
     in fix \again ->
          set >>= \case
            0 -> pure ()
            _ ->
              Libzmq.errno >>= \case
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
send :: MonadIO m => Socket -> NonEmpty ByteString -> m ()
send socket (toList -> messages0) = liftIO do
  flip fix messages0 \loop -> \case
    [message] ->
      sendFrame socket message 0
    message : messages -> do
      sendFrame socket message (Libzmq.zMQ_DONTWAIT .|. Libzmq.zMQ_SNDMORE)
      loop messages
    [] ->
      error "send: []"

sendFrame :: Socket -> ByteString -> CInt -> IO ()
sendFrame socket message flags =
  ByteString.unsafeUseAsCStringLen message \(ptr, fromIntegral -> len) ->
    fix \again ->
      Libzmq.send (unSocket socket) ptr len flags >>= \case
        -1 ->
          Libzmq.errno >>= \case
            EAGAIN -> do
              waitUntilCanSend socket
              again
            EINTR -> again
            errno -> throwError "zmq_send" errno
        _ -> pure () -- Ignore number of bytes sent; why is this interesting?

-- | <http://api.zeromq.org/4-3:zmq-msg-recv>
-- TODO only works for non-threadsafe sockets with a FD
receive :: MonadUnliftIO m => Socket -> m (NonEmpty ByteString)
receive socket =
  withTemporaryFrame \frame ->
    liftIO (receive_ frame socket)

receive_ :: Frame -> Socket -> IO (NonEmpty ByteString)
receive_ frame socket =
  receiveFrame_ frame socket >>= loop []
  where
    loop :: [ByteString] -> ByteString -> IO (NonEmpty ByteString)
    loop acc1 acc0 =
      isLastFrame frame >>= \case
        False -> do
          part <- receiveFrame_ frame socket
          loop (part : acc1) acc0
        True ->
          pure (acc0 :| reverse acc1)

receiveFrame_ :: Frame -> Socket -> IO ByteString
receiveFrame_ frame socket =
  fix \again ->
    doReceiveFrame >>= \case
      -1 ->
        Libzmq.errno >>= \case
          EAGAIN -> do
            waitUntilCanReceive socket
            again
          EINTR -> again
          errno -> throwError "zmq_msg_recv" errno
      _len -> copyFrameBytes frame
  where
    doReceiveFrame =
      Libzmq.receiveFrame (unFrame frame) (unSocket socket) Libzmq.zMQ_DONTWAIT

waitUntilCanReceive :: Socket -> IO ()
waitUntilCanReceive =
  waitUntilCan 1 -- FIXME use zmq_poller api, not deprecated ZMQ_POLLIN

waitUntilCanSend :: Socket -> IO ()
waitUntilCanSend =
  waitUntilCan 2 -- FIXME use zmq_poller api, not deprecated ZMQ_POLLOUT

waitUntilCan :: CInt -> Socket -> IO ()
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
    when (state .&. events == 0) again
