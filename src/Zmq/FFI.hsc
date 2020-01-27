{-# LANGUAGE CPP #-}

module Zmq.FFI where

#include <zmq.h>

import Data.Coerce (coerce)
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

type Context = Ptr ()
type Poller  = Ptr ()
type Socket  = Ptr ()

newtype Message
  = Message { unMessage :: Ptr () }

instance Storable Message where
  alignment _ = #{alignment zmq_msg_t}
  sizeOf _ = #{size zmq_msg_t}

  peek :: Ptr Message -> IO Message
  peek =
    coerce
      @( Ptr ( Ptr CChar ) -> IO ( Ptr CChar ) )
      #{ peek zmq_msg_t, _ }

  poke :: Ptr Message -> Message -> IO ()
  poke =
    coerce
      @( Ptr ( Ptr CChar ) -> Ptr CChar -> IO () )
      #{ poke zmq_msg_t, _ }

eADDRINUSE      :: Errno
eADDRNOTAVAIL   :: Errno
eAFNOSUPPORT    :: Errno
eCONNABORTED    :: Errno
eCONNREFUSED    :: Errno
eCONNRESET      :: Errno
eFSM            :: Errno
eHOSTUNREACH    :: Errno
eINPROGRESS     :: Errno
eMSGSIZE        :: Errno
eMTHREAD        :: Errno
eNETDOWN        :: Errno
eNETRESET       :: Errno
eNETUNREACH     :: Errno
eNOBUFS         :: Errno
eNOCOMPATPROTO  :: Errno
eNOTCONN        :: Errno
eNOTSOCK        :: Errno
eNOTSUP         :: Errno
ePROTONOSUPPORT :: Errno
eTERM           :: Errno
eTIMEDOUT       :: Errno
eADDRINUSE      = Errno ( #const EADDRINUSE )
eADDRNOTAVAIL   = Errno ( #const EADDRNOTAVAIL )
eAFNOSUPPORT    = Errno ( #const EAFNOSUPPORT )
eCONNABORTED    = Errno ( #const ECONNABORTED )
eCONNREFUSED    = Errno ( #const ECONNREFUSED )
eCONNRESET      = Errno ( #const ECONNRESET )
eFSM            = Errno ( #const EFSM )
eHOSTUNREACH    = Errno ( #const EHOSTUNREACH )
eINPROGRESS     = Errno ( #const EINPROGRESS )
eMSGSIZE        = Errno ( #const EMSGSIZE )
eMTHREAD        = Errno ( #const EMTHREAD )
eNETDOWN        = Errno ( #const ENETDOWN )
eNETRESET       = Errno ( #const ENETRESET )
eNETUNREACH     = Errno ( #const ENETUNREACH )
eNOBUFS         = Errno ( #const ENOBUFS )
eNOCOMPATPROTO  = Errno ( #const ENOCOMPATPROTO )
eNOTCONN        = Errno ( #const ENOTCONN )
eNOTSOCK        = Errno ( #const ENOTSOCK )
eNOTSUP         = Errno ( #const ENOTSUP )
ePROTONOSUPPORT = Errno ( #const EPROTONOSUPPORT )
eTERM           = Errno ( #const ETERM )
eTIMEDOUT       = Errno ( #const ETIMEDOUT )


zMQ_DONTWAIT  :: CInt
zMQ_EVENTS    :: CInt
zMQ_FD        :: CInt
zMQ_MORE      :: CInt
zMQ_POLLIN    :: CInt
zMQ_POLLOUT   :: CInt
zMQ_PUB       :: CInt
zMQ_SUB       :: CInt
zMQ_SUBSCRIBE :: CInt
zMQ_DONTWAIT  = #const ZMQ_DONTWAIT
zMQ_EVENTS    = #const ZMQ_EVENTS
zMQ_FD        = #const ZMQ_FD
zMQ_MORE      = #const ZMQ_MORE
zMQ_POLLIN    = #const ZMQ_POLLIN
zMQ_POLLOUT   = #const ZMQ_POLLOUT
zMQ_PUB       = #const ZMQ_PUB
zMQ_SUB       = #const ZMQ_SUB
zMQ_SUBSCRIBE = #const ZMQ_SUBSCRIBE


foreign import ccall safe "zmq_bind"
  zmq_bind :: Socket -> CString -> IO CInt

foreign import ccall unsafe "&zmq_close"
  zmq_close :: FunPtr ( Socket -> IO () )

foreign import ccall safe "zmq_connect"
  zmq_connect :: Socket -> CString -> IO CInt

foreign import ccall unsafe "zmq_ctx_new"
  zmq_ctx_new :: IO Context

foreign import ccall safe "zmq_ctx_term"
  zmq_ctx_term :: Context -> IO CInt

foreign import ccall safe "zmq_disconnect"
  zmq_disconnect :: Socket -> CString -> IO CInt

foreign import ccall unsafe "zmq_errno"
  zmq_errno :: IO Errno

foreign import ccall unsafe "zmq_getsockopt"
  zmq_getsockopt :: Socket -> CInt -> Ptr a -> Ptr CSize -> IO CInt

foreign import ccall unsafe "zmq_msg_data"
  zmq_msg_data :: Ptr Message -> IO ( Ptr CChar )

foreign import ccall unsafe "zmq_msg_close"
  zmq_msg_close :: Ptr Message -> IO CInt

foreign import ccall unsafe "zmq_msg_get"
  zmq_msg_get :: Ptr Message -> CInt -> IO CInt

foreign import ccall unsafe "zmq_msg_init"
  zmq_msg_init :: Ptr Message -> IO CInt

foreign import ccall unsafe "zmq_msg_recv"
  zmq_msg_recv :: Ptr Message -> Socket -> CInt -> IO CInt

-- foreign import ccall unsafe "zmq_poller_destroy"
--   zmq_poller_destroy :: Ptr Poller -> IO CInt

-- foreign import ccall unsafe "zmq_poller_new"
--   zmq_poller_new :: IO Poller

foreign import ccall safe "zmq_send"
  zmq_send :: Socket -> Ptr a -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "zmq_setsockopt"
  zmq_setsockopt :: Socket -> CInt -> Ptr a -> CSize -> IO CInt

foreign import ccall unsafe "zmq_socket"
  zmq_socket :: Context -> CInt -> IO Socket

foreign import ccall safe "zmq_unbind"
  zmq_unbind :: Socket -> CString -> IO CInt
