module Zmq.Context
  ( context
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

setMaxSockets
  :: Natural
  -> IO ()
setMaxSockets =
  void . FFI.zmq_ctx_set context FFI.zMQ_MAX_SOCKETS . fromIntegral
