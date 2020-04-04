module Zmq.Context
  ( Context(..)
  , Options(..)
  , defaultOptions
  , newContext
  , Zmqhs.terminateContext
  , setIoThreads
  , setMaxSockets
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

newContext
  :: MonadIO m
  => Options
  -> m Context
newContext options = do
  context <- Zmqhs.newContext
  setIoThreads context ( ioThreads options )
  setMaxSockets context ( maxSockets options )
  pure context


-- TODO move to Zmq.API.CtxSet

setIoThreads
  :: MonadIO m
  => Zmqhs.Context
  -> Natural
  -> m ()
setIoThreads =
  setNatural Zmqhs.ioThreads

setMaxSockets
  :: MonadIO m
  => Zmqhs.Context
  -> Natural
  -> m ()
setMaxSockets =
  setNatural Zmqhs.maxSockets


----------------------------------------------------------------------------------

setNatural
  :: MonadIO m
  => Zmqhs.ContextOption
  -> Zmqhs.Context
  -> Natural
  -> m ()
setNatural option context =
  void . Zmqhs.setContextOption context option . fromIntegral
