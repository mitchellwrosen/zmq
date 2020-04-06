module Zmq.XPublisher
  ( XPublisher

  , open
  , close
  , with

  , bind
  , unbind

  , connect
  , disconnect

  , send
  , receive
  ) where

import qualified UnliftIO

import qualified Zmqhs

import Zmq.Context
import Zmq.Endpoint
import Zmq.Internal (renderEndpoint)
import Zmq.Prelude
import Zmq.SubscriptionMessage


newtype XPublisher
  = XPublisher { unXPublisher :: MVar Zmqhs.Socket }
  deriving stock ( Eq )

open :: MonadIO m => Context -> m XPublisher
open context = do
  socket <- Zmqhs.open context Zmqhs.XPub
  socketVar <- UnliftIO.newMVar socket
  pure ( XPublisher socketVar )

close :: MonadUnliftIO m => XPublisher -> m ()
close publisher =
  UnliftIO.withMVar ( unXPublisher publisher ) Zmqhs.close

with :: MonadUnliftIO m => Context -> ( XPublisher -> m a ) -> m a
with context =
  UnliftIO.bracket ( open context ) close

bind :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
bind publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \socket ->
    Zmqhs.bind socket ( renderEndpoint endpoint )

unbind :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
unbind publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \socket ->
    Zmqhs.unbind socket ( renderEndpoint endpoint )

connect :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
connect publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \socket ->
    Zmqhs.connect socket ( renderEndpoint endpoint )

disconnect :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
disconnect publisher endpoint =
  UnliftIO.withMVar ( unXPublisher publisher ) \socket ->
    Zmqhs.disconnect socket ( renderEndpoint endpoint )

send :: MonadUnliftIO m => XPublisher -> NonEmpty ByteString -> m ()
send publisher message =
  UnliftIO.withMVar ( unXPublisher publisher ) \socket ->
    Zmqhs.send socket message

receive :: MonadUnliftIO m => XPublisher -> m SubscriptionMessage
receive publisher =
  UnliftIO.withMVar ( unXPublisher publisher ) \socket ->
    fix \again ->
      Zmqhs.receive socket >>= \case
        UnsubscribeMessage prefix -> pure ( Unsubscribe prefix )
        SubscribeMessage prefix -> pure ( Subscribe prefix )
        _ -> again
