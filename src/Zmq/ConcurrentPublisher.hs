module Zmq.ConcurrentPublisher
  ( ConcurrentPublisher

  , open

  , bind

  , send
  ) where

import Zmq.Context (contextVar)
import Zmq.Endpoint
import Zmq.Error
import Zmq.ManagedSocket (ManagedSocket)
import Zmq.Prelude
import qualified Zmq.API.Bind as API
import qualified Zmq.FFI as FFI
import qualified Zmq.ManagedSocket as ManagedSocket


newtype ConcurrentPublisher
  = ConcurrentPublisher ManagedSocket
  deriving newtype ( Eq, Ord, Show )

open
  :: MonadIO m
  => m ( Maybe ConcurrentPublisher )
open = liftIO do
  context <- readMVar contextVar
  coerce ( ManagedSocket.open context FFI.zMQ_PUB )

bind
  :: MonadIO m
  => ConcurrentPublisher
  -> Endpoint transport
  -> m ( Either API.BindError () )
bind publisher endpoint =
  liftIO ( coerce ManagedSocket.bind publisher endpoint )

send
  :: MonadIO m
  => ConcurrentPublisher
  -> NonEmpty ByteString
  -> m ()
send publisher message = liftIO do
  action :: STM ( IO ( Either CInt () ) ) <-
    coerce ManagedSocket.send publisher message

  -- TODO ConcurrentPublisher don't block on send
  await :: IO ( Either CInt () ) <-
    atomically action

  await >>= \case
    Left errno ->
      bugUnexpectedErrno "zmq_send" errno

    Right () ->
      pure ()
