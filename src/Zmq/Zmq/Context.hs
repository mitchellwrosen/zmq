module Zmq.Context
  ( Context(..)
  , Options(..)
  , defaultOptions
  , newContext
  , terminateContext
  , setIoThreads
  , setMaxSockets
  ) where

import qualified Zmq.FFI as FFI

import qualified Zmqhs

import Zmq.Prelude
import qualified Zmq.API.CtxTerm as API


newtype Context
  = Context { unContext :: Zmqhs.Context }

data Options
  = Options
  { ioThreads :: Natural
  , maxSockets :: Natural
  }

defaultOptions :: Options
defaultOptions =
  Options
    { ioThreads = fromIntegral FFI.zMQ_IO_THREADS_DFLT
    , maxSockets = fromIntegral FFI.zMQ_MAX_SOCKETS_DFLT
    }

newContext
  :: MonadIO m
  => Options
  -> m Context
newContext options = liftIO do
  context <- Zmqhs.newContext
  setIoThreads context ( ioThreads options )
  setMaxSockets context ( maxSockets options )
  pure ( Context context )

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
