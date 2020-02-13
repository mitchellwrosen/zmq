{-# LANGUAGE CPP #-}

module Zmq.FFI where

#include <zmq.h>

import Foreign.C
import Foreign.Ptr

import Libzmq.Socket


newtype Poller = Poller ()

-- foreign import ccall unsafe "zmq_poller_destroy"
--   zmq_poller_destroy :: Ptr ( Ptr Poller ) -> IO CInt

-- foreign import ccall unsafe "zmq_poller_new"
--   zmq_poller_new :: IO ( Ptr Poller )

foreign import ccall safe "zmq_send"
  zmq_send :: Ptr Socket -> Ptr a -> CSize -> CInt -> IO CInt
