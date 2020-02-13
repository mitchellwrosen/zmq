{-# LANGUAGE CPP #-}

module Zmq.FFI where

#include <zmq.h>

import Foreign.C
import Foreign.Ptr

import Libzmq.Socket


newtype Poller = Poller ()

zMQ_DONTWAIT         :: CInt
zMQ_IO_THREADS_DFLT  :: CInt
zMQ_MAX_SOCKETS_DFLT :: CInt
zMQ_MORE             :: CInt
zMQ_POLLIN           :: CInt
zMQ_POLLOUT          :: CInt
zMQ_SNDMORE          :: CInt
zMQ_DONTWAIT         = #const ZMQ_DONTWAIT
zMQ_IO_THREADS_DFLT  = #const ZMQ_IO_THREADS_DFLT
zMQ_MAX_SOCKETS_DFLT = #const ZMQ_MAX_SOCKETS_DFLT
zMQ_MORE             = #const ZMQ_MORE
zMQ_POLLIN           = #const ZMQ_POLLIN
zMQ_POLLOUT          = #const ZMQ_POLLOUT
zMQ_SNDMORE          = #const ZMQ_SNDMORE


-- foreign import ccall unsafe "zmq_poller_destroy"
--   zmq_poller_destroy :: Ptr ( Ptr Poller ) -> IO CInt

-- foreign import ccall unsafe "zmq_poller_new"
--   zmq_poller_new :: IO ( Ptr Poller )

foreign import ccall safe "zmq_send"
  zmq_send :: Ptr Socket -> Ptr a -> CSize -> CInt -> IO CInt
