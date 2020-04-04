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
import Zmq.Prelude
import Zmq.SubscriptionMessage
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Connect as API
import qualified Zmq.API.Disconnect as API
import qualified Zmq.API.Recv as API
import qualified Zmq.API.Send as API
import qualified Zmq.API.Unbind as API


newtype XPublisher
  = XPublisher { unXPublisher :: MVar Zmqhs.Socket }
  deriving stock ( Eq )

open :: MonadIO m => Context -> m XPublisher
open context = do
  sock <- Zmqhs.socket context Zmqhs.XPub
  sockVar <- UnliftIO.newMVar sock
  pure ( XPublisher sockVar )

close :: MonadUnliftIO m => XPublisher -> m ()
close publisher =
  UnliftIO.withMVar ( unXPublisher publisher ) Zmqhs.close

bind :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
bind publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \sock ->
    liftIO ( API.bind sock endpoint )

unbind :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
unbind publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \sock ->
    liftIO ( API.unbind sock endpoint )

connect :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
connect publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \sock ->
    liftIO ( API.connect sock endpoint )

disconnect :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
disconnect publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \sock ->
    liftIO ( API.disconnect sock endpoint )

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
