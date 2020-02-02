module Zmq.ManagedSocket
  ( ManagedSocket
  , bind
  , open
  , send
  ) where

import Data.Function (on)
import Data.Ord (comparing)

import Zmq.Endpoint
import Zmq.Prelude
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Send as API
import qualified Zmq.API.Socket as API
import qualified Zmq.FFI as FFI

-- TODO ManagedSocket close

-- TODO ManagedSocket recv


data ManagedSocket
  = ManagedSocket
  { canary :: IORef ()
  , socket :: Ptr FFI.Socket

  , bind
      :: forall transport.
         Endpoint transport
      -> IO ( Either API.BindError () )

  , send
      :: NonEmpty ByteString
      -> IO ( STM ( IO ( Either CInt () ) ) )
  }

instance Eq ManagedSocket where
  (==) = (==) `on` socket

instance Ord ManagedSocket where
  compare = comparing socket

instance Show ManagedSocket where
  show = show . socket

data Request where
  RequestBind
    :: Endpoint transport
    -> MVar ( Either API.BindError () )
    -> Request

  RequestSend
    :: NonEmpty ByteString
    -> MVar ( Either CInt () )
    -> Request

open
  :: Ptr FFI.Context
  -> CInt
  -> IO ( Maybe ManagedSocket )
open context socketType =
  mask_ do
    openVar <- newEmptyMVar
    threadId <- spawnManagerThread context socketType openVar

    readMVar openVar >>= \case
      Nothing ->
        pure Nothing

      Just ( socket, requestQueue ) -> do
        let
          bind
            :: Endpoint transport
            -> IO ( Either API.BindError () )
          bind endpoint = do
            responseVar <- newEmptyMVar
            atomically do
              writeTBQueue requestQueue ( RequestBind endpoint responseVar )
            readMVar responseVar

        let
          send
            :: NonEmpty ByteString
            -> IO ( STM ( IO ( Either CInt () ) ) )
          send message = do
            responseVar <- newEmptyMVar
            pure do
              writeTBQueue requestQueue ( RequestSend message responseVar )
              pure ( takeMVar responseVar )

        canary <- newIORef ()
        ( void . mkWeakIORef canary ) do
          FFI.zmq_close socket
          killThread threadId

        pure ( Just ManagedSocket{..} )

spawnManagerThread
  :: Ptr FFI.Context
  -> CInt
  -> MVar ( Maybe ( Ptr FFI.Socket, TBQueue Request ) )
  -> IO ThreadId
spawnManagerThread context socketType openVar =
  forkIOWithUnmask \unmask ->
    unmask do
      API.socket' context socketType >>= \case
        Nothing ->
          putMVar openVar Nothing

        Just sock -> do
          requestQueue :: TBQueue Request <-
            newTBQueueIO 1024 -- TODO better magic num based on HWM?

          putMVar openVar ( Just ( sock, requestQueue ) )

          forever do
            atomically ( readTBQueue requestQueue ) >>= \case
              RequestBind endpoint responseVar ->
                API.bind sock endpoint >>= putMVar responseVar

              RequestSend message responseVar ->
                API.nonBlockingSend sock message >>= putMVar responseVar
