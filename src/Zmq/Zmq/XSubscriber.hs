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

import qualified Libzmq

import qualified Zmqhs

import Zmq.Context
import Zmq.Endpoint
import Zmq.Prelude
import Zmq.SubscriptionMessage (SubscriptionMessage(..))
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Connect as API
import qualified Zmq.API.Disconnect as API
import qualified Zmq.API.Recv as API
import qualified Zmq.API.Send as API
import qualified Zmq.API.Socket as API
import qualified Zmq.API.Unbind as API
import qualified Zmq.SubscriptionMessage as SubscriptionMessage


newtype XSubscriber
  = XSubscriber { unXSubscriber :: Ptr Libzmq.Socket }
  deriving newtype ( Eq, Ord, Show )

open
  :: MonadIO m
  => Context
  -> m XSubscriber
open context = liftIO do
  coerce ( API.socket ( unContext context ) Zmqhs.xSUB )

close
  :: MonadIO m
  => XSubscriber
  -> m ()
close =
  liftIO . coerce Libzmq.close

bind
  :: MonadIO m
  => XSubscriber
  -> Endpoint transport
  -> m ()
bind subscriber endpoint = liftIO do
  API.bind ( unXSubscriber subscriber ) endpoint

unbind
  :: MonadIO m
  => XSubscriber
  -> Endpoint transport
  -> m ()
unbind subscriber endpoint = liftIO do
  API.unbind ( unXSubscriber subscriber ) endpoint

connect
  :: MonadIO m
  => XSubscriber
  -> Endpoint transport
  -> m ()
connect subscriber endpoint = liftIO do
  API.connect ( unXSubscriber subscriber ) endpoint

disconnect
  :: MonadIO m
  => XSubscriber
  -> Endpoint transport
  -> m ()
disconnect subscriber endpoint = liftIO do
  API.disconnect ( unXSubscriber subscriber ) endpoint

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
send subscriber message = liftIO do
  API.sendThatNeverBlocks
    ( unXSubscriber subscriber )
    ( SubscriptionMessage.serialize message :| [] )

recv
  :: MonadIO m
  => XSubscriber
  -> m ( NonEmpty ByteString )
recv =
  liftIO . coerce API.nonThreadsafeRecv
