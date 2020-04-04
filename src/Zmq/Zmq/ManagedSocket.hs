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

-- TODO ManagedSocket recv


data ManagedSocket a
  = ManagedSocket
  { socket :: Zmqhs.Socket
  , close :: IO ()
  , bind :: forall transport. Endpoint transport -> IO ()
  , send :: NonEmpty ByteString -> IO ( STM ( IO a ) )
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
    -> MVar ( Either Zmqhs.Error () )
    -> Request a

  RequestClose
    :: Request a

  RequestSend
    :: NonEmpty ByteString
    -> MVar ( Either Zmqhs.Error a )
    -> Request a


open
  :: forall a.
     Context
  -> Zmqhs.SocketType
  -> ( Zmqhs.Socket -> NonEmpty ByteString -> IO a )
  -> IO ( ManagedSocket a )
open context socketType sendImpl = do
  openVar <- newEmptyMVar
  _ <- SlaveThread.fork ( runManagerThread context socketType sendImpl openVar )
  readMVar openVar >>= \case
    Left ex ->
      throwIO ex

    Right ( socket, requestQueue ) -> do
      pure ManagedSocket
        { bind =
            \endpoint -> do
              responseVar <- newEmptyMVar
              atomically do
                writeTBQueue requestQueue ( RequestBind endpoint responseVar )
              takeMVar responseVar >>= either throwIO pure
        , close = atomically ( writeTBQueue requestQueue RequestClose )
        , send =
            \message -> do
              responseVar <- newEmptyMVar
              pure do
                writeTBQueue requestQueue ( RequestSend message responseVar )
                pure ( takeMVar responseVar >>= either throwIO pure )
        , socket = socket
        }

runManagerThread
  :: forall a.
     Context
  -> Zmqhs.SocketType
  -> ( Zmqhs.Socket -> NonEmpty ByteString -> IO a )
  -> MVar ( Either Zmqhs.Error ( Zmqhs.Socket, TBQueue ( Request a ) ) )
  -> IO ()
runManagerThread context socketType sendImpl openVar =
  try ( Zmqhs.socket context socketType ) >>= \case
    Left err -> putMVar openVar ( Left err )
    Right sock -> do
      -- TODO better magic num based on HWM?
      requestQueue <- newTBQueueIO 1024
      putMVar openVar ( Right ( sock, requestQueue ) )
      unsafeUnmask do
        loopManagerThread
          SocketHandle
            { doBind = API.bind sock
            , doClose = Zmqhs.close sock
            , doSend = sendImpl sock
            }
          ( atomically ( readTBQueue requestQueue ) )

data SocketHandle a
  = SocketHandle
  { doBind :: forall transport. Endpoint transport -> IO ()
  , doClose :: IO ()
  , doSend :: NonEmpty ByteString -> IO a
  }

loopManagerThread
  :: SocketHandle a
  -> IO ( Request a )
  -> IO ()
loopManagerThread sock awaitRequest =
  fix \loop -> do
    awaitRequest >>= \case
      RequestBind endpoint responseVar -> do
        result <- try ( doBind sock endpoint )
        putMVar responseVar result
        loop

      RequestClose ->
        doClose sock

      RequestSend message responseVar -> do
        result <- try ( doSend sock message )
        putMVar responseVar result
        loop
