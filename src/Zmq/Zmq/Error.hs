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

import qualified Libzmq

import Zmq.Prelude


pattern EADDRINUSE :: CInt
pattern EADDRINUSE <- ((== Libzmq.eADDRINUSE) -> True)

pattern EADDRNOTAVAIL :: CInt
pattern EADDRNOTAVAIL <- ((== Libzmq.eADDRNOTAVAIL) -> True)

pattern EAGAIN :: CInt
pattern EAGAIN <- ((== coerce eAGAIN) -> True)

pattern EHOSTUNREACH :: CInt
pattern EHOSTUNREACH <- ((== Libzmq.eHOSTUNREACH) -> True)

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
pattern EMTHREAD <- ((== Libzmq.eMTHREAD) -> True)

pattern ENODEV :: CInt
pattern ENODEV <- ((== coerce eNODEV) -> True)

pattern ENOENT :: CInt
pattern ENOENT <- ((== coerce eNOENT) -> True)

pattern ENOTSOCK :: CInt
pattern ENOTSOCK <- ((== Libzmq.eNOTSOCK) -> True) where
  ENOTSOCK = Libzmq.eNOTSOCK

pattern ETERM :: CInt
pattern ETERM <- ((== Libzmq.eTERM) -> True) where
  ETERM = Libzmq.eTERM
