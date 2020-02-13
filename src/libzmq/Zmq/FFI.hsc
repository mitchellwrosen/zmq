{-# LANGUAGE CPP #-}

module Zmq.FFI where

#include <zmq.h>

import Data.Coerce (coerce)
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import Libzmq.Socket


newtype Poller = Poller ()

newtype Frame
  = Frame { unFrame :: Ptr () }

instance Storable Frame where
  alignment _ = #{alignment zmq_msg_t}
  sizeOf _ = #{size zmq_msg_t}

  peek :: Ptr Frame -> IO Frame
  peek =
    coerce
      @( Ptr ( Ptr CChar ) -> IO ( Ptr CChar ) )
      #{ peek zmq_msg_t, _ }

  poke :: Ptr Frame -> Frame -> IO ()
  poke =
    coerce
      @( Ptr ( Ptr CChar ) -> Ptr CChar -> IO () )
      #{ poke zmq_msg_t, _ }

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


foreign import ccall unsafe "zmq_msg_data"
  zmq_msg_data :: Ptr Frame -> IO ( Ptr CChar )

foreign import ccall unsafe "zmq_msg_close"
  zmq_msg_close :: Ptr Frame -> IO CInt

foreign import ccall unsafe "zmq_msg_get"
  zmq_msg_get :: Ptr Frame -> CInt -> IO CInt

foreign import ccall unsafe "zmq_msg_init"
  zmq_msg_init :: Ptr Frame -> IO CInt

foreign import ccall unsafe "zmq_msg_recv"
  zmq_msg_recv :: Ptr Frame -> Ptr Socket -> CInt -> IO CInt

-- foreign import ccall unsafe "zmq_poller_destroy"
--   zmq_poller_destroy :: Ptr ( Ptr Poller ) -> IO CInt

-- foreign import ccall unsafe "zmq_poller_new"
--   zmq_poller_new :: IO ( Ptr Poller )

foreign import ccall safe "zmq_send"
  zmq_send :: Ptr Socket -> Ptr a -> CSize -> CInt -> IO CInt
