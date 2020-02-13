module Zmq.Error
  ( pattern EADDRINUSE
  , pattern EADDRNOTAVAIL
  , pattern EAGAIN
  , pattern EFAULT
  , pattern EHOSTUNREACH
  , pattern EINTR
  , pattern EINVAL
  , pattern EMFILE
  , pattern EMTHREAD
  , pattern ENODEV
  , pattern ENOENT
  , pattern ENOTSOCK
  , pattern ETERM
  ) where

import Foreign.C

import Zmq.FFI
import Zmq.Prelude


pattern EADDRINUSE :: CInt
pattern EADDRINUSE <- ((== Zmq.FFI.eADDRINUSE) -> True)

pattern EADDRNOTAVAIL :: CInt
pattern EADDRNOTAVAIL <- ((== Zmq.FFI.eADDRNOTAVAIL) -> True)

pattern EAGAIN :: CInt
pattern EAGAIN <- ((== coerce eAGAIN) -> True)

pattern EHOSTUNREACH :: CInt
pattern EHOSTUNREACH <- ((== Zmq.FFI.eHOSTUNREACH) -> True)

pattern EINTR :: CInt
pattern EINTR <- ((== coerce eINTR) -> True)

pattern EINVAL :: CInt
pattern EINVAL <- ((== coerce eINVAL) -> True) where
  EINVAL = coerce eINVAL

pattern EFAULT :: CInt
pattern EFAULT <- ((== coerce eFAULT) -> True) where
  EFAULT = coerce eFAULT

pattern EMFILE :: CInt
pattern EMFILE <- ((== coerce eMFILE) -> True)

pattern EMTHREAD :: CInt
pattern EMTHREAD <- ((== Zmq.FFI.eMTHREAD) -> True)

pattern ENODEV :: CInt
pattern ENODEV <- ((== coerce eNODEV) -> True)

pattern ENOENT :: CInt
pattern ENOENT <- ((== coerce eNOENT) -> True)

pattern ENOTSOCK :: CInt
pattern ENOTSOCK <- ((== Zmq.FFI.eNOTSOCK) -> True) where
  ENOTSOCK = Zmq.FFI.eNOTSOCK

pattern ETERM :: CInt
pattern ETERM <- ((== Zmq.FFI.eTERM) -> True) where
  ETERM = Zmq.FFI.eTERM
