module Zmq.Context
  ( contextVar
  , setIoThreads
  , setMaxSockets
  ) where

import System.IO.Unsafe (unsafePerformIO)

import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | Global context.
contextVar :: MVar ( Ptr FFI.Context )
contextVar =
  unsafePerformIO newEmptyMVar
{-# NOINLINE contextVar #-}

setIoThreads
  :: Ptr FFI.Context
  -> Natural
  -> IO ()
setIoThreads =
  setNatural FFI.zMQ_IO_THREADS

setMaxSockets
  :: Ptr FFI.Context
  -> Natural
  -> IO ()
setMaxSockets =
  setNatural FFI.zMQ_MAX_SOCKETS


----------------------------------------------------------------------------------

setNatural
  :: FFI.Contextopt
  -> Ptr FFI.Context
  -> Natural
  -> IO ()
setNatural option context =
  void . FFI.zmq_ctx_set context option . fromIntegral
