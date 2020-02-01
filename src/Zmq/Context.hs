module Zmq.Context
  ( context
  , setIoThreads
  , setMaxSockets
  ) where

import System.IO.Unsafe (unsafePerformIO)

import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | Global context.
context :: Ptr ()
context =
  unsafePerformIO FFI.zmq_ctx_new
{-# NOINLINE context #-}

setIoThreads
  :: Natural
  -> IO ()
setIoThreads =
  setNatural FFI.zMQ_IO_THREADS

setMaxSockets
  :: Natural
  -> IO ()
setMaxSockets =
  setNatural FFI.zMQ_MAX_SOCKETS


--------------------------------------------------------------------------------

setNatural
  :: CInt
  -> Natural
  -> IO ()
setNatural option =
  void . FFI.zmq_ctx_set context option . fromIntegral
