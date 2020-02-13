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

import Zmq.Context
import Zmq.Endpoint
import Zmq.Prelude
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Connect as API
import qualified Zmq.API.Disconnect as API
import qualified Zmq.API.Send as API
import qualified Zmq.API.Socket as API
import qualified Zmq.API.Unbind as API
import qualified Zmq.FFI as FFI


newtype Publisher
  = Publisher { unPublisher :: Ptr FFI.Socket }
  deriving newtype ( Eq, Ord, Show )

open
  :: MonadIO m
  => Context
  -> m Publisher
open context = liftIO do
  coerce ( API.socket ( unContext context ) FFI.zMQ_PUB )

close
  :: MonadIO m
  => Publisher
  -> m ()
close =
  liftIO . coerce FFI.zmq_close

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
