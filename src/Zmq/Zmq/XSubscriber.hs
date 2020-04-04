module Zmq.XSubscriber
  ( XSubscriber

  , open
  , close

  , bind
  , unbind

  , connect
  , disconnect

  , subscribe
  , unsubscribe

  , recv
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified UnliftIO

import qualified Zmqhs

import Zmq.Context
import Zmq.Endpoint
import Zmq.Internal (renderEndpoint)
import Zmq.Prelude
import Zmq.SubscriptionMessage (SubscriptionMessage(..))
import qualified Zmq.API.Connect as API
import qualified Zmq.API.Disconnect as API
import qualified Zmq.API.Recv as API
import qualified Zmq.API.Send as API
import qualified Zmq.SubscriptionMessage as SubscriptionMessage


newtype XSubscriber
  = XSubscriber { unXSubscriber :: MVar Zmqhs.Socket }
  deriving stock ( Eq )

open :: MonadIO m => Context -> m XSubscriber
open context = do
  sock <- Zmqhs.open context Zmqhs.XSub
  sockVar <- UnliftIO.newMVar sock
  pure ( XSubscriber sockVar )

close :: MonadUnliftIO m => XSubscriber -> m ()
close subscriber =
  UnliftIO.withMVar ( unXSubscriber subscriber ) Zmqhs.close

bind :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
bind subscriber endpoint =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \sock ->
    Zmqhs.bind sock ( renderEndpoint endpoint )

unbind :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
unbind subscriber endpoint =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \sock ->
    Zmqhs.unbind sock ( renderEndpoint endpoint )

connect :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
connect subscriber endpoint =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \sock ->
    liftIO ( API.connect sock endpoint )

disconnect :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
disconnect subscriber endpoint =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \sock ->
    liftIO ( API.disconnect sock endpoint )

subscribe :: MonadUnliftIO m => XSubscriber -> ByteString -> m ()
subscribe subscriber prefix =
  send subscriber ( Subscribe prefix )

unsubscribe :: MonadUnliftIO m => XSubscriber -> ByteString -> m ()
unsubscribe subscriber prefix =
  send subscriber ( Unsubscribe prefix )

send :: MonadUnliftIO m => XSubscriber -> SubscriptionMessage -> m ()
send subscriber message =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \sock ->
  liftIO do
    API.sendThatNeverBlocks
      sock
      ( SubscriptionMessage.serialize message :| [] )

recv :: MonadUnliftIO m => XSubscriber -> m ( NonEmpty ByteString )
recv subscriber =
  UnliftIO.withMVar ( unXSubscriber subscriber ) \sock ->
    liftIO ( API.nonThreadsafeRecv sock )
