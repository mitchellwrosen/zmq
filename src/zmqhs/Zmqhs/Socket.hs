module Zmqhs.Socket
  ( Socket(..)

  , open
  , close

  , bind
  , unbind

  , connect
  , disconnect

  , getSocketEvents
  , getSocketFd
  , setSocketSubscribe
  , setSocketUnsubscribe
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.Function (fix)
import Foreign.C (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke, sizeOf)
import qualified Data.ByteString.Unsafe as ByteString

import qualified Libzmq

import Zmqhs.Context (Context(..))
import Zmqhs.Endpoint (Endpoint, withEndpoint)
import Zmqhs.Internal.Error
import Zmqhs.SocketType (SocketType(..))


newtype Socket
  = Socket
  { unSocket :: Ptr Libzmq.Socket }
  deriving stock ( Eq, Ord, Show )

-- | <http://api.zeromq.org/4-3:zmq-socket>
--
-- May throw:
--   * @EMFILE@ if no more sockets can be opened.
--   * @ETERM@ if the context was terminated.
open :: MonadIO m => Context -> SocketType -> m Socket
open context socketType = liftIO do
  sock <- Libzmq.socket ( unContext context ) ( socketTypeToCInt socketType )
  if sock /= nullPtr
    then pure ( Socket sock )
    else Libzmq.errno >>= throwError "zmq_socket"

  where
    socketTypeToCInt :: SocketType -> CInt
    socketTypeToCInt = \case
      Pub -> Libzmq.pub
      Sub -> Libzmq.sub
      XPub -> Libzmq.xpub
      XSub -> Libzmq.xsub

-- | <http://api.zeromq.org/4-3:zmq-close>
close :: MonadIO m => Socket -> m ()
close sock = liftIO do
  Libzmq.close ( unSocket sock )

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
    Libzmq.bind ( unSocket sock ) endpoint' >>= \case
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
    Libzmq.unbind ( unSocket sock ) endpoint' >>= \case
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
    Libzmq.connect ( unSocket sock ) endpoint' >>= \case
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
    Libzmq.disconnect ( unSocket sock ) endpoint' >>= \case
      0 -> pure ()
      _ ->
        Libzmq.errno >>= \case
          ENOENT -> pure () -- The endpoint was not connected.
          errno -> throwError "zmq_disconnect" errno

getSocketEvents :: Socket -> IO ( Either CInt CInt )
getSocketEvents =
  getIntSocketOption Libzmq.events

getSocketFd :: Socket -> IO ( Either CInt CInt )
getSocketFd =
  getIntSocketOption Libzmq.fd

getIntSocketOption :: CInt -> Socket -> IO ( Either CInt CInt )
getIntSocketOption option sock =
  alloca \intPtr ->
    alloca \sizePtr -> do
      poke sizePtr ( fromIntegral ( sizeOf ( undefined :: CInt ) ) )

      Libzmq.getSocketOption ( unSocket sock ) option intPtr sizePtr >>= \case
        0 -> Right <$> peek intPtr
        _ -> Left <$> Libzmq.errno

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @EINVAL@ if the option is invalid.
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
setSocketSubscribe :: MonadIO m => Socket -> ByteString -> m ()
setSocketSubscribe =
  setBinarySocketOption Libzmq.subscribe

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @EINVAL@ if the option is invalid.
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
setSocketUnsubscribe :: MonadIO m => Socket -> ByteString -> m ()
setSocketUnsubscribe =
  setBinarySocketOption Libzmq.unsubscribe

setBinarySocketOption :: MonadIO m => CInt -> Socket -> ByteString -> m ()
setBinarySocketOption option sock bytes = liftIO do
  ByteString.unsafeUseAsCStringLen bytes \( bytes', len ) ->
    let
      set =
        Libzmq.setSocketOption
          ( unSocket sock ) option bytes' ( fromIntegral len )
    in
      fix \again ->
        set >>= \case
          0 -> pure ()
          _ ->
            Libzmq.errno >>= \case
              EINTR -> again
              errno -> throwError "zmq_setsockopt" errno
