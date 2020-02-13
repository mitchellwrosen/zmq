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
  setNatural FFI.zMQ_IO_THREADS

setMaxSockets
  :: Zmqhs.Context
  -> Natural
  -> IO ()
setMaxSockets =
  setNatural FFI.zMQ_MAX_SOCKETS


----------------------------------------------------------------------------------

setNatural
  :: FFI.Contextopt
  -> Zmqhs.Context
  -> Natural
  -> IO ()
setNatural option ( Zmqhs.Context context ) =
    void . FFI.zmq_ctx_set context option . fromIntegral
