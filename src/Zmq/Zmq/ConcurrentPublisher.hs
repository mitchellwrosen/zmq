{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Zmq.ConcurrentPublisher
  ( ConcurrentPublisher

  , open
  , close

  , bind

  , send
  ) where

import qualified Zmqhs

import Zmq.Context
import Zmq.Endpoint
import Zmq.ManagedSocket (ManagedSocket)
import Zmq.Prelude
import qualified Zmq.API.Send as API
import qualified Zmq.ManagedSocket as ManagedSocket


newtype ConcurrentPublisher
  = ConcurrentPublisher { unConcurrentPublisher :: ManagedSocket () }
  deriving newtype ( Eq, Ord, Show )

open
  :: MonadIO m
  => Context
  -> m ConcurrentPublisher
open context = liftIO do
  coerce ( ManagedSocket.open context Zmqhs.pub API.sendThatNeverBlocks )

close
  :: MonadIO m
  => ConcurrentPublisher
  -> m ()
close =
  liftIO . coerce @( ManagedSocket () -> IO () ) ManagedSocket.close

bind
  :: MonadIO m
  => ConcurrentPublisher
  -> Endpoint transport
  -> m ()
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
