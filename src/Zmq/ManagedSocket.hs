{-# LANGUAGE MagicHash #-}

module Zmq.ManagedSocket
  ( ManagedSocket
  , open
  , close

  , bind

  , send
  ) where

import Data.Function (on)
import Data.Ord (comparing)
import GHC.IO (unsafeUnmask)
import qualified SlaveThread

import Zmq.Endpoint
import Zmq.Prelude
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Socket as API
import qualified Zmq.FFI as FFI

-- TODO ManagedSocket close

-- TODO ManagedSocket recv


data ManagedSocket a
  = ManagedSocket
  { socket :: Ptr FFI.Socket

  , close :: IO ()

  , bind
      :: forall transport.
         Endpoint transport
      -> IO ( Either API.BindError () )

  , send
      :: NonEmpty ByteString
      -> IO ( STM ( IO a ) )
  }

instance Eq ( ManagedSocket a ) where
  (==) = (==) `on` socket

instance Ord ( ManagedSocket a ) where
  compare = comparing socket

instance Show ( ManagedSocket a ) where
  show = show . socket

data Request a where
  RequestBind
    :: Endpoint transport
    -> MVar ( Either API.BindError () )
    -> Request a

  RequestClose
    :: Request a

  RequestSend
    :: NonEmpty ByteString
    -> MVar a
    -> Request a

open
  :: forall a.
     Ptr FFI.Context
  -> FFI.Socktype
  -> ( Ptr FFI.Socket -> NonEmpty ByteString -> IO a )
  -> IO ( Maybe ( ManagedSocket a ) )
open context socketType sendImpl =
  mask_ do
    openVar <- newEmptyMVar
    void ( spawnManagerThread context socketType sendImpl openVar )

    readMVar openVar >>= \case
      Nothing ->
        pure Nothing

      Just ( socket, requestQueue ) -> do
        let
          close :: IO ()
          close =
            atomically ( writeTBQueue requestQueue RequestClose )

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
            -> IO ( STM ( IO a ) )
          send message = do
            responseVar <- newEmptyMVar
            pure do
              writeTBQueue requestQueue ( RequestSend message responseVar )
              pure do
                response <- takeMVar responseVar
                pure response

        pure ( Just ManagedSocket{..} )

spawnManagerThread
  :: forall a.
     Ptr FFI.Context
  -> FFI.Socktype
  -> ( Ptr FFI.Socket -> NonEmpty ByteString -> IO a )
  -> MVar ( Maybe ( Ptr FFI.Socket, TBQueue ( Request a ) ) )
  -> IO ThreadId
spawnManagerThread context socketType sendImpl openVar =
  SlaveThread.fork do
    unsafeUnmask do
      API.socket' context socketType >>= \case
        Nothing ->
          putMVar openVar Nothing

        Just sock -> do
          requestQueue :: TBQueue ( Request a ) <-
            newTBQueueIO 1024 -- TODO better magic num based on HWM?

          putMVar openVar ( Just ( sock, requestQueue ) )

          fix \loop -> do
            atomically ( readTBQueue requestQueue ) >>= \case
              RequestBind endpoint responseVar -> do
                result <- API.bind sock endpoint
                putMVar responseVar result
                loop

              RequestClose ->
                FFI.zmq_close sock

              RequestSend message responseVar -> do
                response <- sendImpl sock message
                putMVar responseVar response
                loop
