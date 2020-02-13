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

import qualified Zmqhs

import Zmq.Context (Context)
import Zmq.Endpoint
import Zmq.Prelude
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Socket as API

-- TODO ManagedSocket close

-- TODO ManagedSocket recv


data ManagedSocket a
  = ManagedSocket
  { socket :: Zmqhs.Socket

  , close :: IO ()

  , bind
      :: forall transport.
         Endpoint transport
      -> IO ()

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
    -> MVar ( Maybe SomeException )
    -> Request a

  RequestClose
    :: Request a

  RequestSend
    :: NonEmpty ByteString
    -> MVar ( Either SomeException a )
    -> Request a

open
  :: forall a.
     Context
  -> Zmqhs.SocketType
  -> ( Zmqhs.Socket -> NonEmpty ByteString -> IO a )
  -> IO ( ManagedSocket a )
open context socketType sendImpl =
  mask_ do
    openVar <- newEmptyMVar
    void ( spawnManagerThread context socketType sendImpl openVar )

    readMVar openVar >>= \case
      Left ex ->
        throwIO ex

      Right ( socket, requestQueue ) -> do
        let
          close :: IO ()
          close =
            atomically ( writeTBQueue requestQueue RequestClose )

        let
          bind
            :: Endpoint transport
            -> IO ()
          bind endpoint = do
            responseVar <- newEmptyMVar
            atomically do
              writeTBQueue requestQueue ( RequestBind endpoint responseVar )
            readMVar responseVar >>= maybe ( pure () ) throwIO

        let
          send
            :: NonEmpty ByteString
            -> IO ( STM ( IO a ) )
          send message = do
            responseVar <- newEmptyMVar
            pure do
              writeTBQueue requestQueue ( RequestSend message responseVar )
              pure ( takeMVar responseVar >>= either throwIO pure )

        pure ManagedSocket{..}

spawnManagerThread
  :: forall a.
     Context
  -> Zmqhs.SocketType
  -> ( Zmqhs.Socket -> NonEmpty ByteString -> IO a )
  -> MVar ( Either SomeException ( Zmqhs.Socket, TBQueue ( Request a ) ) )
  -> IO ThreadId
spawnManagerThread context socketType sendImpl openVar =
  SlaveThread.fork do
    unsafeUnmask do
      try ( API.socket context socketType ) >>= \case
        Left ex ->
          case fromException @SomeAsyncException ex of
            Nothing -> putMVar openVar ( Left ex )
            Just _ -> throwIO ex

        Right sock -> do
          requestQueue :: TBQueue ( Request a ) <-
            newTBQueueIO 1024 -- TODO better magic num based on HWM?

          putMVar openVar ( Right ( sock, requestQueue ) )

          fix \loop -> do
            atomically ( readTBQueue requestQueue ) >>= \case
              RequestBind endpoint responseVar -> do
                try ( API.bind sock endpoint ) >>= \case
                  Left ex ->
                    case fromException @SomeAsyncException ex of
                      Nothing -> do
                        putMVar responseVar ( Just ex )
                        loop
                      Just _ ->
                        throwIO ex

                  Right () -> do
                    putMVar responseVar Nothing
                    loop

              RequestClose ->
                Zmqhs.close sock

              RequestSend message responseVar -> do
                try ( sendImpl sock message ) >>= \case
                  Left ex ->
                    case fromException @SomeAsyncException ex of
                      Nothing -> do
                        putMVar responseVar ( Left ex )
                        loop

                      Just _ ->
                        throwIO ex

                  Right result -> do
                    putMVar responseVar ( Right result )
                    loop
