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

import Zmq.Endpoint
import Zmq.Error
import Zmq.Prelude
import Zmq.SubscriptionMessage
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Close as API
import qualified Zmq.API.Connect as API
import qualified Zmq.API.Disconnect as API
import qualified Zmq.API.Recv as API
import qualified Zmq.API.Send as API
import qualified Zmq.API.Socket as API
import qualified Zmq.API.Unbind as API
import qualified Zmq.FFI as FFI


newtype XPublisher
  = XPublisher { unXPublisher :: ForeignPtr FFI.Socket }
  deriving stock ( Eq, Data, Ord, Show )

open
  :: MonadIO m
  => m ( Maybe XPublisher )
open =
  liftIO ( coerce ( API.socket FFI.zMQ_XPUB ) )

close
  :: MonadIO m
  => XPublisher
  -> m ()
close publisher =
  liftIO ( coerce API.close publisher )

bind
  :: MonadIO m
  => XPublisher
  -> Endpoint transport
  -> m ( Either API.BindError () )
bind publisher endpoint =
  liftIO ( coerce API.bind publisher endpoint )

unbind
  :: MonadIO m
  => XPublisher
  -> Endpoint transport
  -> m ()
unbind publisher endpoint =
  liftIO ( coerce API.unbind publisher endpoint )

connect
  :: MonadIO m
  => XPublisher
  -> Endpoint transport
  -> m ( Either API.ConnectError () )
connect publisher endpoint =
  liftIO ( coerce API.connect publisher endpoint )

disconnect
  :: MonadIO m
  => XPublisher
  -> Endpoint transport
  -> m ()
disconnect publisher endpoint =
  liftIO ( coerce API.disconnect publisher endpoint )

send
  :: MonadIO m
  => XPublisher
  -> NonEmpty ByteString
  -> m ()
send publisher message = liftIO do
  coerce API.nonBlockingSend publisher message >>= \case
    Left errno ->
      bugUnexpectedErrno "zmq_send" errno

    Right () ->
      pure ()

recv
  :: MonadIO m
  => XPublisher
  -> m SubscriptionMessage
recv publisher = liftIO do
  fix \again ->
    coerce API.nonThreadsafeRecv publisher >>= \case
      UnsubscribeMessage prefix ->
        pure ( Unsubscribe prefix )
      SubscribeMessage prefix ->
        pure ( Subscribe prefix )
      _ ->
        again
