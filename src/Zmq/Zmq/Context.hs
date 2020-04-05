module Zmq.Context
  ( newContext
  , Zmqhs.terminateContext
  , withContext
  , Context(..)
  , Options(..)
  , defaultOptions
  ) where

import qualified UnliftIO

import qualified Libzmq

import Zmqhs (Context(..))
import qualified Zmqhs

import Zmq.Prelude


data Options
  = Options
  { ioThreads :: Natural
  , maxMessageSize :: Natural
  , maxSockets :: Natural
  }

defaultOptions :: Options
defaultOptions =
  Options
    { ioThreads = fromIntegral Libzmq.ioThreadsDflt
    , maxMessageSize = fromIntegral @CInt maxBound
    , maxSockets = fromIntegral Libzmq.maxSocketsDflt
    }

-- | Create a new ZMQ context.
newContext :: MonadIO m => Options -> m Context
newContext options = do
  context <- Zmqhs.newContext
  Zmqhs.setContextIoThreads context ( ioThreads options )
  Zmqhs.setContextMaxMessageSize context ( maxMessageSize options )
  Zmqhs.setContextMaxSockets context ( maxSockets options )
  pure context

withContext :: MonadUnliftIO m => Options -> ( Context -> m a ) -> m a
withContext options =
  UnliftIO.bracket ( newContext options ) Zmqhs.terminateContext
