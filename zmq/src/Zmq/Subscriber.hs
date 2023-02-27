module Zmq.Subscriber
  ( Subscriber,
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
import Data.List.NonEmpty (NonEmpty)
import UnliftIO
import Zmq.Context
import Zmq.Endpoint
import Zmq.Internal (renderEndpoint)
import Zmqhs qualified

newtype Subscriber = Subscriber {unSubscriber :: MVar Zmqhs.Socket}
  deriving stock (Eq)

open :: MonadIO m => Context -> m Subscriber
open context = do
  sock <- Zmqhs.open context Zmqhs.Sub
  sockVar <- newMVar sock
  pure (Subscriber sockVar)

close :: MonadUnliftIO m => Subscriber -> m ()
close subscriber =
  withMVar (unSubscriber subscriber) Zmqhs.close

with :: MonadUnliftIO m => Context -> (Subscriber -> m a) -> m a
with context =
  bracket (open context) close

bind :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
bind subscriber endpoint =
  withMVar (unSubscriber subscriber) \sock ->
    Zmqhs.bind sock (renderEndpoint endpoint)

unbind :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
unbind subscriber endpoint =
  withMVar (unSubscriber subscriber) \sock ->
    Zmqhs.unbind sock (renderEndpoint endpoint)

connect :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
connect subscriber endpoint =
  withMVar (unSubscriber subscriber) \sock ->
    Zmqhs.connect sock (renderEndpoint endpoint)

disconnect :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
disconnect subscriber endpoint =
  withMVar (unSubscriber subscriber) \sock ->
    Zmqhs.disconnect sock (renderEndpoint endpoint)

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
subscribe :: MonadUnliftIO m => Subscriber -> ByteString -> m ()
subscribe subscriber prefix =
  withMVar (unSubscriber subscriber) \sock ->
    Zmqhs.setSocketSubscribe sock prefix

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
unsubscribe :: MonadUnliftIO m => Subscriber -> ByteString -> m ()
unsubscribe subscriber prefix =
  withMVar (unSubscriber subscriber) \sock ->
    Zmqhs.setSocketUnsubscribe sock prefix

receive :: MonadUnliftIO m => Subscriber -> m (NonEmpty ByteString)
receive subscriber =
  withMVar (unSubscriber subscriber) \sock ->
    Zmqhs.receive sock
