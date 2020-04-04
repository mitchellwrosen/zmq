module Zmq.Context
  ( Context(..)
  , Options(..)
  , defaultOptions
  , newContext
  , Zmqhs.terminateContext
  ) where

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
newContext
  :: MonadIO m
  => Options
  -> m Context
newContext options = do
  context <- Zmqhs.newContext
  setNatural Zmqhs.ioThreads context ( ioThreads options )
  setNatural Zmqhs.maxSockets context ( maxSockets options )
  pure context


----------------------------------------------------------------------------------

setNatural
  :: MonadIO m
  => Zmqhs.ContextOption
  -> Zmqhs.Context
  -> Natural
  -> m ()
setNatural option context =
  void . Zmqhs.setContextOption context option . fromIntegral
