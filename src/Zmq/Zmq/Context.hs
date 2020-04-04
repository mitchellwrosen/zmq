module Zmq.Context
  ( Context(..)
  , Options(..)
  , defaultOptions
  , newContext
  , terminateContext
  , setIoThreads
  , setMaxSockets
  ) where

import qualified Libzmq

import Zmqhs (Context(..))
import qualified Zmqhs

import Zmq.Prelude
import qualified Zmq.API.CtxTerm as API


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
newContext options = liftIO do
  context <- Zmqhs.newContext
  setIoThreads context ( ioThreads options )
  setMaxSockets context ( maxSockets options )
  pure context

terminateContext
  :: MonadIO m
  => Context
  -> m ()
terminateContext context =
  liftIO ( coerce API.ctxTerm context )


-- TODO move to Zmq.API.CtxSet

setIoThreads
  :: Zmqhs.Context
  -> Natural
  -> IO ()
setIoThreads =
  setNatural Zmqhs.ioThreads

setMaxSockets
  :: Zmqhs.Context
  -> Natural
  -> IO ()
setMaxSockets =
  setNatural Zmqhs.maxSockets


----------------------------------------------------------------------------------

setNatural
  :: Zmqhs.ContextOption
  -> Zmqhs.Context
  -> Natural
  -> IO ()
setNatural option context =
  void . Zmqhs.setContextOption context option . fromIntegral
