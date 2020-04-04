module Zmqhs.Socket
  ( Socket(..)

  , socket
  , close

  , bind
  , unbind

  , connect
  , disconnect

  , getSocketEvents
  , getSocketFd
  , setSocketSubscribe
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Foreign.C (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke, sizeOf)
import qualified Data.ByteString.Unsafe as ByteString

import qualified Libzmq

import Zmqhs.Context (Context(..))
import Zmqhs.Endpoint (Endpoint, withEndpoint)
import Zmqhs.Internal.Error (throwError)
import Zmqhs.SocketType (SocketType(..))


newtype Socket
  = Socket
  { unSocket :: Ptr Libzmq.Socket }
  deriving stock ( Eq, Ord, Show )

-- | <http://api.zeromq.org/4-3:zmq-socket>
--
-- May throw:
--   * @EMFILE@ if no more ZMQ sockets can be opened.
--   * @ETERM@ if the context was terminated.
socket :: MonadIO m => Context -> SocketType -> m Socket
socket context socketType = liftIO do
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

bind :: Socket -> Endpoint -> IO ( Either CInt () )
bind sock endpoint =
  withEndpoint endpoint \endpoint' ->
    Libzmq.bind ( unSocket sock ) endpoint' >>= \case
      0 -> pure ( Right () )
      _ -> Left <$> Libzmq.errno

unbind :: Socket -> Endpoint -> IO ( Either CInt () )
unbind sock endpoint =
  withEndpoint endpoint \endpoint' ->
    Libzmq.unbind ( unSocket sock ) endpoint' >>= \case
      0 -> pure ( Right () )
      _ -> Left <$> Libzmq.errno

connect :: Socket -> Endpoint -> IO ( Either CInt () )
connect sock endpoint =
  withEndpoint endpoint \endpoint' ->
    Libzmq.connect ( unSocket sock ) endpoint' >>= \case
      0 -> pure ( Right () )
      _ -> Left <$> Libzmq.errno

disconnect :: Socket -> Endpoint -> IO ( Either CInt () )
disconnect sock endpoint =
  withEndpoint endpoint \endpoint' ->
    Libzmq.disconnect ( unSocket sock ) endpoint' >>= \case
      0 -> pure ( Right () )
      _ -> Left <$> Libzmq.errno

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

setSocketSubscribe :: Socket -> ByteString -> IO ( Either CInt () )
setSocketSubscribe =
  setBinarySocketOption Libzmq.subscribe

setBinarySocketOption :: CInt -> Socket -> ByteString -> IO ( Either CInt () )
setBinarySocketOption option sock bytes =
  ByteString.unsafeUseAsCStringLen bytes \( bytes', len ) ->
    Libzmq.setSocketOption ( unSocket sock ) option bytes' ( fromIntegral len ) >>= \case
      0 -> pure ( Right () )
      _ -> Left <$> Libzmq.errno
