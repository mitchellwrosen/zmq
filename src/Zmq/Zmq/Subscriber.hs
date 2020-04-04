module Zmq.Subscriber
  ( Subscriber

  , open
  , close

  , bind
  , unbind

  , connect
  , disconnect

  , subscribe

  , recv
  ) where

import qualified UnliftIO

import qualified Zmqhs

import Zmq.Context
import Zmq.Endpoint
import Zmq.Prelude
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Connect as API
import qualified Zmq.API.Disconnect as API
import qualified Zmq.API.Recv as API
import qualified Zmq.API.Subscribe as API
import qualified Zmq.API.Unbind as API


newtype Subscriber
  = Subscriber { unSubscriber :: MVar Zmqhs.Socket }
  deriving stock ( Eq )

open :: MonadIO m => Context -> m Subscriber
open context = do
  sock <- Zmqhs.socket context Zmqhs.Sub
  sockVar <- UnliftIO.newMVar sock
  pure ( Subscriber sockVar )

close :: MonadUnliftIO m => Subscriber -> m ()
close subscriber =
  UnliftIO.withMVar ( unSubscriber subscriber ) Zmqhs.close

bind :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
bind subscriber endpoint =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    liftIO ( API.bind sock endpoint )

unbind :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
unbind subscriber endpoint =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    liftIO ( API.unbind sock endpoint )

connect :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
connect subscriber endpoint =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    liftIO ( API.connect sock endpoint )

disconnect :: MonadUnliftIO m => Subscriber -> Endpoint transport -> m ()
disconnect subscriber endpoint =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    liftIO ( API.disconnect sock endpoint )

subscribe :: MonadUnliftIO m => Subscriber -> ByteString -> m ()
subscribe subscriber prefix =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    liftIO ( API.subscribe sock prefix )

recv :: MonadUnliftIO m => Subscriber -> m ( NonEmpty ByteString )
recv subscriber =
  UnliftIO.withMVar ( unSubscriber subscriber ) \sock ->
    liftIO ( API.nonThreadsafeRecv sock )
