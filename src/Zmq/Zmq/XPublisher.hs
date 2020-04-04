module Zmq.XPublisher
  ( XPublisher

  , open
  , close

  , bind
  , unbind

  , connect
  , disconnect

  , send
  , recv
  ) where

import qualified UnliftIO

import qualified Zmqhs

import Zmq.Context
import Zmq.Endpoint
import Zmq.Internal (renderEndpoint)
import Zmq.Prelude
import Zmq.SubscriptionMessage
import qualified Zmq.API.Recv as API
import qualified Zmq.API.Send as API


newtype XPublisher
  = XPublisher { unXPublisher :: MVar Zmqhs.Socket }
  deriving stock ( Eq )

open :: MonadIO m => Context -> m XPublisher
open context = do
  sock <- Zmqhs.open context Zmqhs.XPub
  sockVar <- UnliftIO.newMVar sock
  pure ( XPublisher sockVar )

close :: MonadUnliftIO m => XPublisher -> m ()
close publisher =
  UnliftIO.withMVar ( unXPublisher publisher ) Zmqhs.close

bind :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
bind publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \sock ->
    Zmqhs.bind sock ( renderEndpoint endpoint )

unbind :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
unbind publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \sock ->
    Zmqhs.unbind sock ( renderEndpoint endpoint )

connect :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
connect publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \sock ->
    Zmqhs.connect sock ( renderEndpoint endpoint )

disconnect :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
disconnect publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \sock ->
    Zmqhs.disconnect sock ( renderEndpoint endpoint )

send :: MonadUnliftIO m => XPublisher -> NonEmpty ByteString -> m ()
send publisher message =
  UnliftIO.withMVar ( unXPublisher publisher ) \sock ->
    liftIO ( API.sendThatNeverBlocks sock message )

recv :: MonadUnliftIO m => XPublisher -> m SubscriptionMessage
recv publisher =
  UnliftIO.withMVar ( unXPublisher publisher ) \sock ->
    fix \again ->
      liftIO ( API.nonThreadsafeRecv sock ) >>= \case
        UnsubscribeMessage prefix -> pure ( Unsubscribe prefix )
        SubscribeMessage prefix -> pure ( Subscribe prefix )
        _ -> again
