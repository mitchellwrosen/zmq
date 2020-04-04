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
  = Subscriber { unSubscriber :: Zmqhs.Socket }
  deriving newtype ( Eq, Ord, Show )

open
  :: MonadIO m
  => Context
  -> m Subscriber
open context =
  Subscriber <$> Zmqhs.socket context Zmqhs.Sub

close
  :: MonadIO m
  => Subscriber
  -> m ()
close ( Subscriber sock ) =
  Zmqhs.close sock

bind
  :: MonadIO m
  => Subscriber
  -> Endpoint transport
  -> m ()
bind subscriber endpoint = liftIO do
  API.bind ( unSubscriber subscriber ) endpoint

unbind
  :: MonadIO m
  => Subscriber
  -> Endpoint transport
  -> m ()
unbind subscriber endpoint =
  liftIO ( coerce API.unbind subscriber endpoint )

connect
  :: MonadIO m
  => Subscriber
  -> Endpoint transport
  -> m ()
connect subscriber endpoint =
  liftIO ( coerce API.connect subscriber endpoint )

disconnect
  :: MonadIO m
  => Subscriber
  -> Endpoint transport
  -> m ()
disconnect subscriber endpoint =
  liftIO ( coerce API.disconnect subscriber endpoint )

subscribe
  :: MonadIO m
  => Subscriber
  -> ByteString
  -> m ()
subscribe subscriber prefix = liftIO do
  API.subscribe ( unSubscriber subscriber ) prefix

recv
  :: MonadIO m
  => Subscriber
  -> m ( NonEmpty ByteString )
recv =
  liftIO . coerce API.nonThreadsafeRecv
