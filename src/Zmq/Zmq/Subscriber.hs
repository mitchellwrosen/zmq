module Zmq.Subscriber
  ( Subscriber

  , open
  , close
  , with

  , bind
  , unbind

  , connect
  , disconnect

  , subscribe
  , unsubscribe

  , recv
  ) where

import qualified UnliftIO

import qualified Zmqhs

import Zmq.Context
import Zmq.Endpoint
import Zmq.Internal (renderEndpoint)
import Zmq.Prelude
import qualified Zmq.API.Recv as API


newtype Subscriber
  = Subscriber { unSubscriber :: MVar Zmqhs.Socket }
  deriving stock ( Eq )

open :: MonadIO m => Context -> m Subscriber
open context = do
  sock <- Zmqhs.open context Zmqhs.Sub
  sockVar <- UnliftIO.newMVar sock
  pure ( Subscriber sockVar )

close :: MonadUnliftIO m => Subscriber -> m ()
close subscriber =
  UnliftIO.withMVar ( unSubscriber subscriber ) Zmqhs.close

with :: MonadUnliftIO m => Context -> ( Subscriber -> m a ) -> m a
with context =
  UnliftIO.bracket ( open context ) close

bind :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
bind subscriber endpoint =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    Zmqhs.bind sock ( renderEndpoint endpoint )

unbind :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
unbind subscriber endpoint =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    Zmqhs.unbind sock ( renderEndpoint endpoint )

connect :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
connect subscriber endpoint =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    Zmqhs.connect sock ( renderEndpoint endpoint )

disconnect :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
disconnect subscriber endpoint =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    Zmqhs.disconnect sock ( renderEndpoint endpoint )

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
subscribe :: MonadUnliftIO m => Subscriber -> ByteString -> m ()
subscribe subscriber prefix =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    Zmqhs.setSocketSubscribe sock prefix

-- | <http://api.zeromq.org/4-3:zmq-setsockopt>
--
-- May throw:
--   * @ETERM@ if the context was terminated.
--   * @ENOTSOCK@ if the socket is invalid.
unsubscribe :: MonadUnliftIO m => Subscriber -> ByteString -> m ()
unsubscribe subscriber prefix =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    Zmqhs.setSocketUnsubscribe sock prefix

recv :: MonadUnliftIO m => Subscriber -> m ( NonEmpty ByteString )
recv subscriber =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    liftIO ( API.nonThreadsafeRecv sock )
