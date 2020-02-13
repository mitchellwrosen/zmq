{-# LANGUAGE CPP #-}

module Libzmq.Error where

#include <zmq.h>

import Foreign.C (CInt(..))
import Foreign.C.String (CString)


foreign import ccall unsafe "zmq_errno"
  errno :: IO CInt

foreign import ccall unsafe "zmq_strerror"
  strerror :: CInt -> CString


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
