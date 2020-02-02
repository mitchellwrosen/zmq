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

import Zmq.Endpoint
import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Close as API
import qualified Zmq.API.Connect as API
import qualified Zmq.API.Disconnect as API
import qualified Zmq.API.Send as API
import qualified Zmq.API.Socket as API
import qualified Zmq.API.Unbind as API
import qualified Zmq.FFI as FFI


newtype Publisher
  = Publisher ( ForeignPtr FFI.Socket )
  deriving newtype ( Eq, Ord, Show )

open
  :: MonadIO m
  => m ( Maybe Publisher )
open =
  liftIO ( coerce ( API.socket FFI.zMQ_PUB ) )

close
  :: MonadIO m
  => Publisher
  -> m ()
close publisher =
  liftIO ( coerce API.close publisher )

bind
  :: MonadIO m
  => Publisher
  -> Endpoint transport
  -> m ( Either API.BindError () )
bind publisher endpoint = liftIO do
  withForeignPtr ( coerce publisher ) \socket ->
    API.bind socket endpoint

unbind
  :: MonadIO m
  => Publisher
  -> Endpoint transport
  -> m ()
unbind publisher endpoint =
  liftIO ( coerce API.unbind publisher endpoint )

connect
  :: MonadIO m
  => Publisher
  -> Endpoint transport
  -> m ( Either API.ConnectError () )
connect publisher endpoint =
  liftIO ( coerce API.connect publisher endpoint )

disconnect
  :: MonadIO m
  => Publisher
  -> Endpoint transport
  -> m ()
disconnect publisher endpoint =
  liftIO ( coerce API.disconnect publisher endpoint )

send
  :: MonadIO m
  => Publisher
  -> NonEmpty ByteString
  -> m ()
send publisher message = liftIO do
  withForeignPtr ( coerce publisher ) \socket ->
    API.nonBlockingSend socket message >>= \case
      Left errno ->
        unexpectedErrno "zmq_send" errno

      Right () ->
        pure ()
