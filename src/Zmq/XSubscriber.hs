module Zmq.XSubscriber
  ( XSubscriber

  , open
  , close

  , bind
  , unbind

  , connect
  , disconnect

  , subscribe
  , unsubscribe

  , recv
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))

import Zmq.Context
import Zmq.Endpoint
import Zmq.Prelude
import Zmq.SubscriptionMessage (SubscriptionMessage(..))
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Close as API
import qualified Zmq.API.Connect as API
import qualified Zmq.API.Disconnect as API
import qualified Zmq.API.Recv as API
import qualified Zmq.API.Send as API
import qualified Zmq.API.Socket as API
import qualified Zmq.API.Unbind as API
import qualified Zmq.FFI as FFI
import qualified Zmq.SubscriptionMessage as SubscriptionMessage


newtype XSubscriber
  = XSubscriber ( ForeignPtr FFI.Socket )
  deriving newtype ( Eq, Ord, Show )

open
  :: MonadIO m
  => Context
  -> m XSubscriber
open context =
  liftIO ( coerce ( API.socket ( unContext context ) FFI.zMQ_XSUB ) )

close
  :: MonadIO m
  => XSubscriber
  -> m ()
close subscriber =
  liftIO ( coerce API.close subscriber )

bind
  :: MonadIO m
  => XSubscriber
  -> Endpoint transport
  -> m ()
bind subscriber endpoint = liftIO do
  withForeignPtr ( coerce subscriber ) \socket ->
    API.bind socket endpoint

unbind
  :: MonadIO m
  => XSubscriber
  -> Endpoint transport
  -> m ()
unbind subscriber endpoint =
  liftIO ( coerce API.unbind subscriber endpoint )

connect
  :: MonadIO m
  => XSubscriber
  -> Endpoint transport
  -> m ()
connect subscriber endpoint =
  liftIO ( coerce API.connect subscriber endpoint )

disconnect
  :: MonadIO m
  => XSubscriber
  -> Endpoint transport
  -> m ()
disconnect subscriber endpoint =
  liftIO ( coerce API.disconnect subscriber endpoint )

subscribe
  :: MonadIO m
  => XSubscriber
  -> ByteString
  -> m ()
subscribe subscriber prefix =
  send subscriber ( Subscribe prefix )

unsubscribe
  :: MonadIO m
  => XSubscriber
  -> ByteString
  -> m ()
unsubscribe subscriber prefix =
  send subscriber ( Unsubscribe prefix )

send
  :: MonadIO m
  => XSubscriber
  -> SubscriptionMessage
  -> m ()
send subscriber ( SubscriptionMessage.serialize -> message ) = liftIO do
  withForeignPtr ( coerce subscriber ) \socket ->
    -- TODO test that Sub sends don't block
    API.sendThatNeverBlocks socket ( message :| [] )

recv
  :: MonadIO m
  => XSubscriber
  -> m ( NonEmpty ByteString )
recv subscriber =
  liftIO ( coerce API.nonThreadsafeRecv subscriber )
