module Zmq.Publisher
  ( Publisher

  , open
  , close
  , with

  , bind
  , unbind

  , connect
  , disconnect

  , send
  ) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import UnliftIO

import qualified Zmqhs

import Zmq.Context
import Zmq.Endpoint
import Zmq.Internal (renderEndpoint)


newtype Publisher
  = Publisher { unPublisher :: MVar Zmqhs.Socket }
  deriving stock ( Eq )

open :: MonadIO m => Context -> m Publisher
open context = do
  sock <- Zmqhs.open context Zmqhs.Pub
  sockVar <- newMVar sock
  pure ( Publisher sockVar )

close :: MonadUnliftIO m => Publisher -> m ()
close publisher =
  withMVar ( unPublisher publisher ) Zmqhs.close

with :: MonadUnliftIO m => Context -> ( Publisher -> m a ) -> m a
with context =
  bracket ( open context ) close

bind :: MonadUnliftIO m => Publisher -> Endpoint transport -> m ()
bind publisher endpoint =
  withMVar ( unPublisher publisher ) \sock ->
    Zmqhs.bind sock ( renderEndpoint endpoint )

unbind :: MonadUnliftIO m => Publisher -> Endpoint transport -> m ()
unbind publisher endpoint =
  withMVar ( unPublisher publisher ) \sock ->
    Zmqhs.unbind sock ( renderEndpoint endpoint )

connect :: MonadUnliftIO m => Publisher -> Endpoint transport -> m ()
connect publisher endpoint = liftIO do
  withMVar ( unPublisher publisher ) \sock ->
    Zmqhs.connect sock ( renderEndpoint endpoint )

disconnect :: MonadUnliftIO m => Publisher -> Endpoint transport -> m ()
disconnect publisher endpoint =
  withMVar ( unPublisher publisher ) \sock ->
    Zmqhs.disconnect sock ( renderEndpoint endpoint )

send :: MonadUnliftIO m => Publisher -> NonEmpty ByteString -> m ()
send publisher message =
  withMVar ( unPublisher publisher ) \sock ->
    Zmqhs.send sock message
