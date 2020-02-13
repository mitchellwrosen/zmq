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

import Data.ByteString (ByteString)
import Foreign.C (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke, sizeOf)
import qualified Data.ByteString.Unsafe as ByteString

import qualified Libzmq
import qualified Zmq.FFI as FFI

import Zmqhs.Context (Context(..))
import Zmqhs.Endpoint (Endpoint, withEndpoint)
import Zmqhs.SocketType (SocketType(..))


newtype Socket
  = Socket
  { unSocket :: Ptr Libzmq.Socket }
  deriving stock ( Eq, Ord, Show )

socket
  :: Context
  -> SocketType
  -> IO ( Either CInt Socket )
socket context socketType = do
  sock <- Libzmq.socket ( unContext context ) ( unSocketType socketType )
  if sock == nullPtr
    then Left <$> FFI.zmq_errno
    else pure ( Right ( Socket sock ) )

close
  :: Socket
  -> IO ()
close sock =
  Libzmq.close ( unSocket sock )

bind
  :: Socket
  -> Endpoint
  -> IO ( Either CInt () )
bind sock endpoint =
  withEndpoint endpoint \endpoint' ->
    Libzmq.bind ( unSocket sock ) endpoint' >>= \case
      0 -> pure ( Right () )
      _ -> Left <$> FFI.zmq_errno

unbind
  :: Socket
  -> Endpoint
  -> IO ( Either CInt () )
unbind sock endpoint =
  withEndpoint endpoint \endpoint' ->
    Libzmq.unbind ( unSocket sock ) endpoint' >>= \case
      0 -> pure ( Right () )
      _ -> Left <$> FFI.zmq_errno

connect
  :: Socket
  -> Endpoint
  -> IO ( Either CInt () )
connect sock endpoint =
  withEndpoint endpoint \endpoint' ->
    Libzmq.connect ( unSocket sock ) endpoint' >>= \case
      0 -> pure ( Right () )
      _ -> Left <$> FFI.zmq_errno

disconnect
  :: Socket
  -> Endpoint
  -> IO ( Either CInt () )
disconnect sock endpoint =
  withEndpoint endpoint \endpoint' ->
    Libzmq.disconnect ( unSocket sock ) endpoint' >>= \case
      0 -> pure ( Right () )
      _ -> Left <$> FFI.zmq_errno

getSocketEvents
  :: Socket
  -> IO ( Either CInt CInt )
getSocketEvents =
  getIntSocketOption Libzmq.events

getSocketFd
  :: Socket
  -> IO ( Either CInt CInt )
getSocketFd =
  getIntSocketOption Libzmq.fd

getIntSocketOption
  :: CInt
  -> Socket
  -> IO ( Either CInt CInt )
getIntSocketOption option sock =
  alloca \intPtr ->
    alloca \sizePtr -> do
      poke sizePtr ( fromIntegral ( sizeOf ( undefined :: CInt ) ) )

      Libzmq.getSocketOption ( unSocket sock ) option intPtr sizePtr >>= \case
        0 -> Right <$> peek intPtr
        _ -> Left <$> FFI.zmq_errno

setSocketSubscribe
  :: Socket
  -> ByteString
  -> IO ( Either CInt () )
setSocketSubscribe =
  setBinarySocketOption Libzmq.subscribe

setBinarySocketOption
  :: CInt
  -> Socket
  -> ByteString
  -> IO ( Either CInt () )
setBinarySocketOption option sock bytes =
  ByteString.unsafeUseAsCStringLen bytes \( bytes', len ) ->
    Libzmq.setSocketOption ( unSocket sock ) option bytes' ( fromIntegral len ) >>= \case
      0 -> pure ( Right () )
      _ -> Left <$> FFI.zmq_errno
