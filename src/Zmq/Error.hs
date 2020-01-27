module Zmq.Error
  ( Error(..)
  , pattern EADDRINUSE_
  , pattern EADDRNOTAVAIL_
  , pattern EAGAIN_
  , pattern EFAULT_
  , pattern EHOSTUNREACH_
  , pattern EINTR_
  , pattern EINVAL_
  , pattern EMFILE_
  , pattern EMTHREAD_
  , pattern ENODEV_
  , pattern ENOENT_
  , pattern ENOTSOCK_
  , pattern ETERM_
  , bug
  , bugIO
  , bugUnexpectedErrno
  , errInvalidContext
  ) where

import Foreign.C

import Zmq.FFI
import Zmq.Function
import Zmq.Prelude


data Error ( function :: Function ) where
  EADDRINUSE      :: ( CanReturnEADDRINUSE      function ~ 'True ) => Error function
  EADDRNOTAVAIL   :: ( CanReturnEADDRNOTAVAIL   function ~ 'True ) => Error function
  EHOSTUNREACH    :: ( CanReturnEHOSTUNREACH    function ~ 'True ) => Error function
  EINVAL          :: ( CanReturnEINVAL          function ~ 'True ) => Error function
  -- EMFILE          :: ( CanReturnEMFILE          function ~ 'True ) => Error function
  EMTHREAD        :: ( CanReturnEMTHREAD        function ~ 'True ) => Error function
  ENODEV          :: ( CanReturnENODEV          function ~ 'True ) => Error function
  -- ENOENT          :: ( CanReturnENOENT          function ~ 'True ) => Error function
  -- EPROTONOSUPPORT :: ( CanReturnEPROTONOSUPPORT function ~ 'True ) => Error function

instance Show ( Error function ) where
  show = \case
    EADDRINUSE -> "EADDRINUSE"
    EADDRNOTAVAIL -> "EADDRNOTAVAIL"
    EHOSTUNREACH -> "EHOSTUNREACH"
    EINVAL -> "EINVAL"
    -- EMFILE -> "EMFILE"
    EMTHREAD -> "EMTHREAD"
    ENODEV -> "ENODEV"
    -- ENOENT          -> "ENOENT"
    -- EPROTONOSUPPORT -> "EPROTONOSUPPORT"


pattern EADDRINUSE_ :: Errno
pattern EADDRINUSE_ <- ((== Zmq.FFI.eADDRINUSE) -> True)

pattern EADDRNOTAVAIL_ :: Errno
pattern EADDRNOTAVAIL_ <- ((== Zmq.FFI.eADDRNOTAVAIL) -> True)

pattern EAGAIN_ :: Errno
pattern EAGAIN_ <- ((== eAGAIN) -> True)

pattern EHOSTUNREACH_ :: Errno
pattern EHOSTUNREACH_ <- ((== Zmq.FFI.eHOSTUNREACH) -> True)

pattern EINTR_ :: Errno
pattern EINTR_ <- ((== eINTR) -> True)

pattern EINVAL_ :: Errno
pattern EINVAL_ <- ((== eINVAL) -> True) where
  EINVAL_ = eINVAL

pattern EFAULT_ :: Errno
pattern EFAULT_ <- ((== eFAULT) -> True)

pattern EMFILE_ :: Errno
pattern EMFILE_ <- ((== eMFILE) -> True)

pattern EMTHREAD_ :: Errno
pattern EMTHREAD_ <- ((== Zmq.FFI.eMTHREAD) -> True)

pattern ENODEV_ :: Errno
pattern ENODEV_ <- ((== eNODEV) -> True)

pattern ENOENT_ :: Errno
pattern ENOENT_ <- ((== eNOENT) -> True)

pattern ENOTSOCK_ :: Errno
pattern ENOTSOCK_ <- ((== Zmq.FFI.eNOTSOCK) -> True) where
  ENOTSOCK_ = Zmq.FFI.eNOTSOCK

pattern ETERM_ :: Errno
pattern ETERM_ <- ((== Zmq.FFI.eTERM) -> True)


bug :: String -> a
bug message =
  error ( "bug: " ++ message )

bugIO :: MonadIO m => String -> m a
bugIO =
  liftIO . evaluate . bug

bugUnexpectedErrno :: MonadIO m => String -> Errno -> m a
bugUnexpectedErrno func ( Errno n ) =
  liftIO ( fail ( func ++ ": unexpected errno " ++ show n ) )

errInvalidContext :: MonadIO m => m a
errInvalidContext =
  liftIO ( fail "Invalid ZeroMQ context. Did you forget to call `Zmq.main`?" )
