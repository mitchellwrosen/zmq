module Zmq.XSubscriber
  ( XSubscriber,
    open,
    close,
    with,
    bind,
    unbind,
    connect,
    disconnect,
    subscribe,
    unsubscribe,
    receive,
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty ((:|)))
import UnliftIO
import Zmq.Context
import Zmq.Endpoint
import Zmq.Internal (renderEndpoint)
import Zmq.SubscriptionMessage (SubscriptionMessage (..))
import Zmq.SubscriptionMessage qualified as SubscriptionMessage
import Zmqhs qualified

newtype XSubscriber = XSubscriber {unXSubscriber :: MVar Zmqhs.Socket}
  deriving stock (Eq)

open :: MonadIO m => Context -> m XSubscriber
open context = do
  socket <- Zmqhs.open context Zmqhs.XSub
  socketVar <- newMVar socket
  pure (XSubscriber socketVar)

close :: MonadUnliftIO m => XSubscriber -> m ()
close subscriber =
  withMVar (unXSubscriber subscriber) Zmqhs.close

with :: MonadUnliftIO m => Context -> (XSubscriber -> m a) -> m a
with context =
  bracket (open context) close

bind :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
bind subscriber endpoint =
  withMVar (unXSubscriber subscriber) \socket ->
    Zmqhs.bind socket (renderEndpoint endpoint)

unbind :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
unbind subscriber endpoint =
  withMVar (unXSubscriber subscriber) \socket ->
    Zmqhs.unbind socket (renderEndpoint endpoint)

connect :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
connect subscriber endpoint =
  withMVar (unXSubscriber subscriber) \socket ->
    Zmqhs.connect socket (renderEndpoint endpoint)

disconnect :: MonadUnliftIO m => XSubscriber -> Endpoint transport -> m ()
disconnect subscriber endpoint =
  withMVar (unXSubscriber subscriber) \socket ->
    Zmqhs.disconnect socket (renderEndpoint endpoint)

subscribe :: MonadUnliftIO m => XSubscriber -> ByteString -> m ()
subscribe subscriber prefix =
  send subscriber (Subscribe prefix)

unsubscribe :: MonadUnliftIO m => XSubscriber -> ByteString -> m ()
unsubscribe subscriber prefix =
  send subscriber (Unsubscribe prefix)

send :: MonadUnliftIO m => XSubscriber -> SubscriptionMessage -> m ()
send subscriber message =
  withMVar (unXSubscriber subscriber) \socket ->
    Zmqhs.send socket (SubscriptionMessage.serialize message :| [])

receive :: MonadUnliftIO m => XSubscriber -> m (NonEmpty ByteString)
receive subscriber =
  withMVar (unXSubscriber subscriber) \socket ->
    Zmqhs.receive socket
