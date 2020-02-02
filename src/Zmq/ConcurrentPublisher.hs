{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Zmq.ConcurrentPublisher
  ( ConcurrentPublisher

  , open
  , close

  , bind

  , send
  ) where

import Zmq.Context (contextVar)
import Zmq.Endpoint
import Zmq.ManagedSocket (ManagedSocket)
import Zmq.Prelude
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Send as API
import qualified Zmq.FFI as FFI
import qualified Zmq.ManagedSocket as ManagedSocket


newtype ConcurrentPublisher
  = ConcurrentPublisher { unConcurrentPublisher :: ManagedSocket () }
  deriving newtype ( Eq, Ord, Show )

open
  :: MonadIO m
  => m ( Maybe ConcurrentPublisher )
open = liftIO do
  context <- readMVar contextVar
  coerce ( ManagedSocket.open context FFI.zMQ_PUB API.sendThatNeverBlocks )

close
  :: MonadIO m
  => ConcurrentPublisher
  -> m ()
close publisher =
  liftIO ( ManagedSocket.close ( unConcurrentPublisher publisher ) )

bind
  :: MonadIO m
  => ConcurrentPublisher
  -> Endpoint transport
  -> m ( Either API.BindError () )
bind publisher endpoint =
  liftIO ( ManagedSocket.bind ( unConcurrentPublisher publisher ) endpoint )

send
  :: MonadIO m
  => ConcurrentPublisher
  -> NonEmpty ByteString
  -> m ()
send publisher message = liftIO do
  action :: STM ( IO () ) <-
    ManagedSocket.send ( unConcurrentPublisher publisher ) message

  -- TODO ConcurrentPublisher don't block on send
  await :: IO () <-
    atomically action

  await
