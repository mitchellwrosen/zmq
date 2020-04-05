module Zmq.XSubscriber
  ( XSubscriber

  , open
  , close
  , with

  , bind
  , unbind

  , connect
  , disconnect

  , subscribe
  , unsubscribe

  , receive
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified UnliftIO

import qualified Zmqhs

import Zmq.Context
import Zmq.Endpoint
import Zmq.Internal (renderEndpoint)
import Zmq.Prelude
import Zmq.SubscriptionMessage (SubscriptionMessage(..))
import qualified Zmq.API.Send as API
import qualified Zmq.SubscriptionMessage as SubscriptionMessage


newtype XSubscriber
  = XSubscriber { unXSubscriber :: MVar Zmqhs.Socket }
  deriving stock ( Eq )

open :: MonadIO m => Context -> m XSubscriber
open context = do
  socket <- Zmqhs.open context Zmqhs.XSub
  socketVar <- UnliftIO.newMVar socket
  pure ( XSubscriber socketVar )

close :: MonadUnliftIO m => XSubscriber -> m ()
close subscriber =
  UnliftIO.withMVar ( unXSubscriber subscriber ) Zmqhs.close

with :: MonadUnliftIO m => Context -> ( XSubscriber -> m a ) -> m a
with context =
  UnliftIO.bracket ( open context ) close

bind :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
bind subscriber endpoint =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \socket ->
    Zmqhs.bind socket ( renderEndpoint endpoint )

unbind :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
unbind subscriber endpoint =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \socket ->
    Zmqhs.unbind socket ( renderEndpoint endpoint )

connect :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
connect subscriber endpoint =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \socket ->
    Zmqhs.connect socket ( renderEndpoint endpoint )

disconnect :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
disconnect subscriber endpoint =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \socket ->
    Zmqhs.disconnect socket ( renderEndpoint endpoint )

subscribe :: MonadUnliftIO m => XSubscriber -> ByteString -> m ()
subscribe subscriber prefix =
  send subscriber ( Subscribe prefix )

unsubscribe :: MonadUnliftIO m => XSubscriber -> ByteString -> m ()
unsubscribe subscriber prefix =
  send subscriber ( Unsubscribe prefix )

send :: MonadUnliftIO m => XSubscriber -> SubscriptionMessage -> m ()
send subscriber message =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \socket ->
    liftIO do
      API.sendThatNeverBlocks
        socket
        ( SubscriptionMessage.serialize message :| [] )

receive :: MonadUnliftIO m => XSubscriber -> m ( NonEmpty ByteString )
receive subscriber =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \socket ->
    Zmqhs.receive socket
