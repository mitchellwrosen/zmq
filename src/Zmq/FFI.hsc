{-# LANGUAGE CPP #-}

module Zmq.FFI where

#include <zmq.h>

import Data.Coerce (coerce)
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

newtype Context = Context ()
newtype Poller  = Poller ()
newtype Socket  = Socket ()

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

eADDRINUSE      :: CInt
eADDRNOTAVAIL   :: CInt
eAFNOSUPPORT    :: CInt
eCONNABORTED    :: CInt
eCONNREFUSED    :: CInt
eCONNRESET      :: CInt
eFSM            :: CInt
eHOSTUNREACH    :: CInt
eINPROGRESS     :: CInt
eMSGSIZE        :: CInt
eMTHREAD        :: CInt
eNETDOWN        :: CInt
eNETRESET       :: CInt
eNETUNREACH     :: CInt
eNOBUFS         :: CInt
eNOCOMPATPROTO  :: CInt
eNOTCONN        :: CInt
eNOTSOCK        :: CInt
eNOTSUP         :: CInt
ePROTONOSUPPORT :: CInt
eTERM           :: CInt
eTIMEDOUT       :: CInt
eADDRINUSE      = #const EADDRINUSE
eADDRNOTAVAIL   = #const EADDRNOTAVAIL
eAFNOSUPPORT    = #const EAFNOSUPPORT
eCONNABORTED    = #const ECONNABORTED
eCONNREFUSED    = #const ECONNREFUSED
eCONNRESET      = #const ECONNRESET
eFSM            = #const EFSM
eHOSTUNREACH    = #const EHOSTUNREACH
eINPROGRESS     = #const EINPROGRESS
eMSGSIZE        = #const EMSGSIZE
eMTHREAD        = #const EMTHREAD
eNETDOWN        = #const ENETDOWN
eNETRESET       = #const ENETRESET
eNETUNREACH     = #const ENETUNREACH
eNOBUFS         = #const ENOBUFS
eNOCOMPATPROTO  = #const ENOCOMPATPROTO
eNOTCONN        = #const ENOTCONN
eNOTSOCK        = #const ENOTSOCK
eNOTSUP         = #const ENOTSUP
ePROTONOSUPPORT = #const EPROTONOSUPPORT
eTERM           = #const ETERM
eTIMEDOUT       = #const ETIMEDOUT

newtype Sockopt
  = Sockopt CInt

zMQ_EVENTS, zMQ_FD, zMQ_SUBSCRIBE :: Sockopt
zMQ_EVENTS    = Sockopt ( #const ZMQ_EVENTS )
zMQ_FD        = Sockopt ( #const ZMQ_FD )
zMQ_SUBSCRIBE = Sockopt ( #const ZMQ_SUBSCRIBE )

zMQ_DONTWAIT         :: CInt
zMQ_IO_THREADS       :: CInt
zMQ_IO_THREADS_DFLT  :: CInt
zMQ_MAX_SOCKETS      :: CInt
zMQ_MAX_SOCKETS_DFLT :: CInt
zMQ_MORE             :: CInt
zMQ_POLLIN           :: CInt
zMQ_POLLOUT          :: CInt
zMQ_PUB              :: CInt
zMQ_SNDMORE          :: CInt
zMQ_SUB              :: CInt
zMQ_XPUB             :: CInt
zMQ_XSUB             :: CInt
zMQ_DONTWAIT         = #const ZMQ_DONTWAIT
zMQ_IO_THREADS       = #const ZMQ_IO_THREADS
zMQ_IO_THREADS_DFLT  = #const ZMQ_IO_THREADS_DFLT
zMQ_MAX_SOCKETS      = #const ZMQ_MAX_SOCKETS
zMQ_MAX_SOCKETS_DFLT = #const ZMQ_MAX_SOCKETS_DFLT
zMQ_MORE             = #const ZMQ_MORE
zMQ_POLLIN           = #const ZMQ_POLLIN
zMQ_POLLOUT          = #const ZMQ_POLLOUT
zMQ_PUB              = #const ZMQ_PUB
zMQ_SNDMORE          = #const ZMQ_SNDMORE
zMQ_SUB              = #const ZMQ_SUB
zMQ_XPUB             = #const ZMQ_XPUB
zMQ_XSUB             = #const ZMQ_XSUB


foreign import ccall safe "zmq_bind"
  zmq_bind :: Ptr Socket -> CString -> IO CInt

foreign import ccall unsafe "zmq_close"
  zmq_close :: Ptr Socket -> IO ()

foreign import ccall unsafe "&zmq_close"
  zmq_close_ptr :: FunPtr ( Ptr Socket -> IO () )

foreign import ccall safe "zmq_connect"
  zmq_connect :: Ptr Socket -> CString -> IO CInt

foreign import ccall unsafe "zmq_ctx_new"
  zmq_ctx_new :: IO ( Ptr Context )

foreign import ccall unsafe "zmq_ctx_set"
  zmq_ctx_set :: Ptr Context -> CInt -> CInt -> IO CInt

foreign import ccall safe "zmq_ctx_term"
  zmq_ctx_term :: Ptr Context -> IO CInt

foreign import ccall safe "zmq_disconnect"
  zmq_disconnect :: Ptr Socket -> CString -> IO CInt

foreign import ccall unsafe "zmq_errno"
  zmq_errno :: IO CInt

foreign import ccall unsafe "zmq_getsockopt"
  zmq_getsockopt :: Ptr Socket -> Sockopt -> Ptr a -> Ptr CSize -> IO CInt

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

foreign import ccall unsafe "zmq_setsockopt"
  zmq_setsockopt :: Ptr Socket -> Sockopt -> Ptr a -> CSize -> IO CInt

foreign import ccall unsafe "zmq_socket"
  zmq_socket :: Ptr Context -> CInt -> IO ( Ptr Socket )

foreign import ccall safe "zmq_unbind"
  zmq_unbind :: Ptr Socket -> CString -> IO CInt
