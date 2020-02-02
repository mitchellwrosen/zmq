module Zmq.ManagedSocket
  ( ManagedSocket
  , open
  , send
  ) where

import Zmq.Prelude
import qualified Zmq.API.Send as API
import qualified Zmq.API.Socket as API
import qualified Zmq.FFI as FFI


data ManagedSocket
  = ManagedSocket
  { canary :: IORef ()
  , socket :: Ptr FFI.Socket
  , send :: NonEmpty ByteString -> IO ( STM ( IO ( Either Errno () ) ) )
  }

data Request
  = RequestSend ( NonEmpty ByteString ) ( MVar ( Either Errno () ) )

open
  :: Ptr FFI.Context
  -> CInt
  -> IO ( Maybe ManagedSocket )
open context socketType =
  mask_ do
    API.socket' context socketType >>= \case
      Nothing ->
        pure Nothing

      Just socket -> do
        Just <$> open_ socket

open_
  :: Ptr FFI.Socket
  -> IO ManagedSocket
open_ socket = do
  requestQueue :: TBQueue Request <-
    newTBQueueIO 1024 -- TODO better magic num based on HWM?

  threadId :: ThreadId <-
    spawnWorkerThread
      requestQueue
      ( API.nonBlockingSend socket )

  let
    send
      :: NonEmpty ByteString
      -> IO ( STM ( IO ( Either Errno () ) ) )
    send message = do
      responseVar <- newEmptyMVar
      pure do
        writeTBQueue requestQueue ( RequestSend message responseVar )
        pure ( takeMVar responseVar )

  canary <- newIORef ()
  ( void . mkWeakIORef canary ) do
    FFI.zmq_close socket
    killThread threadId

  pure ManagedSocket{..}

spawnWorkerThread
  :: TBQueue Request
  -> ( NonEmpty ByteString -> IO ( Either Errno () ) )
  -> IO ThreadId
spawnWorkerThread requestQueue send_ =
  forkIOWithUnmask \unmask ->
    unmask do
      forever do
        atomically ( readTBQueue requestQueue ) >>= \case
          RequestSend message responseVar ->
            send_ message >>= putMVar responseVar
