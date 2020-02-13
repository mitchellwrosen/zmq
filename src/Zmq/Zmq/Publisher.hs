module Zmq.Publisher
  ( Publisher

  , open
  , close

  , bind
  , unbind

  , connect
  , disconnect

  , send
  ) where

import qualified Libzmq

import qualified Zmqhs

import Zmq.Context
import Zmq.Endpoint
import Zmq.Prelude
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Connect as API
import qualified Zmq.API.Disconnect as API
import qualified Zmq.API.Send as API
import qualified Zmq.API.Socket as API
import qualified Zmq.API.Unbind as API


newtype Publisher
  = Publisher { unPublisher :: Ptr Libzmq.Socket }
  deriving newtype ( Eq, Ord, Show )

open
  :: MonadIO m
  => Context
  -> m Publisher
open context = liftIO do
  coerce ( API.socket ( unContext context ) Zmqhs.pUB )

close
  :: MonadIO m
  => Publisher
  -> m ()
close =
  liftIO . coerce Libzmq.close

bind
  :: MonadIO m
  => Publisher
  -> Endpoint transport
  -> m ()
bind publisher endpoint = liftIO do
  API.bind ( unPublisher publisher ) endpoint

unbind
  :: MonadIO m
  => Publisher
  -> Endpoint transport
  -> m ()
unbind publisher endpoint = liftIO do
  API.unbind ( unPublisher publisher ) endpoint

connect
  :: MonadIO m
  => Publisher
  -> Endpoint transport
  -> m ()
connect publisher endpoint = liftIO do
  API.connect ( unPublisher publisher ) endpoint

disconnect
  :: MonadIO m
  => Publisher
  -> Endpoint transport
  -> m ()
disconnect publisher endpoint = liftIO do
  API.disconnect ( unPublisher publisher ) endpoint

send
  :: MonadIO m
  => Publisher
  -> NonEmpty ByteString
  -> m ()
send publisher message = liftIO do
  API.sendThatNeverBlocks ( unPublisher publisher ) message
