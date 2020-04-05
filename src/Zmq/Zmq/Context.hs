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
  , maxSockets :: Natural
  }

defaultOptions :: Options
defaultOptions =
  Options
    { ioThreads = fromIntegral Libzmq.ioThreadsDflt
    , maxSockets = fromIntegral Libzmq.maxSocketsDflt
    }

-- | Create a new ZMQ context.
newContext :: MonadIO m => Options -> m Context
newContext options = do
  context <- Zmqhs.newContext
  setNatural Zmqhs.ioThreads context ( ioThreads options )
  setNatural Zmqhs.maxSockets context ( maxSockets options )
  pure context

withContext :: MonadUnliftIO m => Options -> ( Context -> m a ) -> m a
withContext options =
  UnliftIO.bracket ( newContext options ) Zmqhs.terminateContext


----------------------------------------------------------------------------------

setNatural
  :: MonadIO m
  => Zmqhs.ContextOption
  -> Zmqhs.Context
  -> Natural
  -> m ()
setNatural option context =
  void . Zmqhs.setContextOption context option . fromIntegral
