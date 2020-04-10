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

import Data.ByteString (ByteString)
import Data.Function
import Data.List.NonEmpty (NonEmpty)
import UnliftIO

import qualified Zmqhs

import Zmq.Context
import Zmq.Endpoint
import Zmq.Internal (renderEndpoint)
import Zmq.SubscriptionMessage


newtype XPublisher
  = XPublisher { unXPublisher :: MVar Zmqhs.Socket }
  deriving stock ( Eq )

open :: MonadIO m => Context -> m XPublisher
open context = do
  socket <- Zmqhs.open context Zmqhs.XPub
  socketVar <- newMVar socket
  pure ( XPublisher socketVar )

close :: MonadUnliftIO m => XPublisher -> m ()
close publisher =
  withMVar ( unXPublisher publisher ) Zmqhs.close

with :: MonadUnliftIO m => Context -> ( XPublisher -> m a ) -> m a
with context =
  bracket ( open context ) close

bind :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
bind publisher endpoint =
  withMVar ( unXPublisher publisher ) \socket ->
    Zmqhs.bind socket ( renderEndpoint endpoint )

unbind :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
unbind publisher endpoint =
  withMVar ( unXPublisher publisher ) \socket ->
    Zmqhs.unbind socket ( renderEndpoint endpoint )

connect :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
connect publisher endpoint =
  withMVar ( unXPublisher publisher ) \socket ->
    Zmqhs.connect socket ( renderEndpoint endpoint )

disconnect :: MonadUnliftIO m => XPublisher -> Endpoint transport -> m ()
disconnect publisher endpoint =
  withMVar ( unXPublisher publisher ) \socket ->
    Zmqhs.disconnect socket ( renderEndpoint endpoint )

send :: MonadUnliftIO m => XPublisher -> NonEmpty ByteString -> m ()
send publisher message =
  withMVar ( unXPublisher publisher ) \socket ->
    Zmqhs.send socket message

receive :: MonadUnliftIO m => XPublisher -> m SubscriptionMessage
receive publisher =
  withMVar ( unXPublisher publisher ) \socket ->
    fix \again ->
      Zmqhs.receive socket >>= \case
        UnsubscribeMessage prefix -> pure ( Unsubscribe prefix )
        SubscribeMessage prefix -> pure ( Subscribe prefix )
        _ -> again
