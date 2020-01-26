module Zmq.Context
  ( context
  ) where

import System.IO.Unsafe (unsafePerformIO)

import Zmq.FFI (zmq_ctx_new)
import Zmq.Prelude


-- | Global context.
context :: Ptr ()
context =
  unsafePerformIO zmq_ctx_new
{-# NOINLINE context #-}

