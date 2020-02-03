module Zmq.XPublisher
  ( XPublisher

  , open
  , close

  , bind
  , unbind

  , connect
  , disconnect

  , send
  , recv
  ) where

import Zmq.Context
import Zmq.Endpoint
import Zmq.Prelude
import Zmq.SubscriptionMessage
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Connect as API
import qualified Zmq.API.Disconnect as API
import qualified Zmq.API.Recv as API
import qualified Zmq.API.Send as API
import qualified Zmq.API.Socket as API
import qualified Zmq.API.Unbind as API
import qualified Zmq.FFI as FFI


newtype XPublisher
  = XPublisher { unXPublisher :: Ptr FFI.Socket }
  deriving newtype ( Eq, Ord, Show )

open
  :: MonadIO m
  => Context
  -> m XPublisher
open context = liftIO do
  coerce ( API.socket ( unContext context ) FFI.zMQ_XPUB )

close
  :: MonadIO m
  => XPublisher
  -> m ()
close =
  liftIO . coerce FFI.zmq_close

bind
  :: MonadIO m
  => XPublisher
  -> Endpoint transport
  -> m ()
bind publisher endpoint = liftIO do
  API.bind ( unXPublisher publisher ) endpoint

unbind
  :: MonadIO m
  => XPublisher
  -> Endpoint transport
  -> m ()
unbind publisher endpoint = liftIO do
  API.unbind ( unXPublisher publisher ) endpoint

connect
  :: MonadIO m
  => XPublisher
  -> Endpoint transport
  -> m ()
connect publisher endpoint = liftIO do
  API.connect ( unXPublisher publisher ) endpoint

disconnect
  :: MonadIO m
  => XPublisher
  -> Endpoint transport
  -> m ()
disconnect publisher endpoint = liftIO do
  API.disconnect ( unXPublisher publisher ) endpoint

send
  :: MonadIO m
  => XPublisher
  -> NonEmpty ByteString
  -> m ()
send publisher message = liftIO do
  API.sendThatNeverBlocks ( unXPublisher publisher ) message

recv
  :: MonadIO m
  => XPublisher
  -> m SubscriptionMessage
recv publisher = liftIO do
  fix \again ->
    API.nonThreadsafeRecv ( unXPublisher publisher ) >>= \case
      UnsubscribeMessage prefix ->
        pure ( Unsubscribe prefix )
      SubscribeMessage prefix ->
        pure ( Subscribe prefix )
      _ ->
        again
