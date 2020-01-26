{-# LANGUAGE CPP #-}

module Zmq.FFI where

#include <zmq.h>

import Foreign.C
import Foreign.Ptr

type Context
  = Ptr ()

type Socket
  = Ptr ()

eADDRINUSE :: Errno
eADDRINUSE =
  Errno ( #const EADDRINUSE )

eADDRNOTAVAIL :: Errno
eADDRNOTAVAIL =
  Errno ( #const EADDRNOTAVAIL )

eAFNOSUPPORT :: Errno
eAFNOSUPPORT =
  Errno ( #const EAFNOSUPPORT )

eCONNABORTED :: Errno
eCONNABORTED =
  Errno ( #const ECONNABORTED )

eCONNREFUSED :: Errno
eCONNREFUSED =
  Errno ( #const ECONNREFUSED )

eCONNRESET :: Errno
eCONNRESET =
  Errno ( #const ECONNRESET )

eFSM :: Errno
eFSM =
  Errno ( #const EFSM )

eHOSTUNREACH :: Errno
eHOSTUNREACH =
  Errno ( #const EHOSTUNREACH )

eINPROGRESS :: Errno
eINPROGRESS =
  Errno ( #const EINPROGRESS )

eMSGSIZE :: Errno
eMSGSIZE =
  Errno ( #const EMSGSIZE )

eMTHREAD :: Errno
eMTHREAD =
  Errno ( #const EMTHREAD )

eNETDOWN :: Errno
eNETDOWN =
  Errno ( #const ENETDOWN )

eNETRESET :: Errno
eNETRESET =
  Errno ( #const ENETRESET )

eNETUNREACH :: Errno
eNETUNREACH =
  Errno ( #const ENETUNREACH )

eNOBUFS :: Errno
eNOBUFS =
  Errno ( #const ENOBUFS )

eNOCOMPATPROTO :: Errno
eNOCOMPATPROTO =
  Errno ( #const ENOCOMPATPROTO )

eNOTCONN :: Errno
eNOTCONN =
  Errno ( #const ENOTCONN )

eNOTSOCK :: Errno
eNOTSOCK =
  Errno ( #const ENOTSOCK )

eNOTSUP :: Errno
eNOTSUP =
  Errno ( #const ENOTSUP )

ePROTONOSUPPORT :: Errno
ePROTONOSUPPORT =
  Errno ( #const EPROTONOSUPPORT )

eTERM :: Errno
eTERM =
  Errno ( #const ETERM )

eTIMEDOUT :: Errno
eTIMEDOUT =
  Errno ( #const ETIMEDOUT )

zMQ_PUB :: CInt
zMQ_PUB =
  #const ZMQ_PUB

zMQ_SUB :: CInt
zMQ_SUB =
  #const ZMQ_SUB

zMQ_SUBSCRIBE :: CInt
zMQ_SUBSCRIBE =
  #const ZMQ_SUBSCRIBE

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

foreign import ccall unsafe "zmq_setsockopt"
  zmq_setsockopt :: Socket -> CInt -> Ptr a -> CSize -> IO CInt

foreign import ccall unsafe "zmq_socket"
  zmq_socket :: Context -> CInt -> IO Socket

foreign import ccall safe "zmq_unbind"
  zmq_unbind :: Socket -> CString -> IO CInt
