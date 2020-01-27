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
  , CanReturnEADDRINUSE
  , CanReturnEADDRNOTAVAIL
  , CanReturnEHOSTUNREACH
  , CanReturnEINVAL
  , CanReturnEMTHREAD
  , CanReturnENODEV
  , bug
  , bugIO
  , bugUnexpectedErrno
  , errInvalidContext
  ) where

import Foreign.C
import GHC.TypeLits (Symbol)

import Zmq.FFI
import Zmq.Prelude


data Error ( function :: Symbol ) where
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


type family CanReturnEADDRINUSE ( function :: Symbol ) :: Bool where
  CanReturnEADDRINUSE "bind" = 'True
  CanReturnEADDRINUSE _ = 'False

type family CanReturnEADDRNOTAVAIL ( function :: Symbol ) :: Bool where
  CanReturnEADDRNOTAVAIL "bind" = 'True
  CanReturnEADDRNOTAVAIL _ = 'False

type family CanReturnEHOSTUNREACH ( function :: Symbol ) :: Bool where
  -- TODO only a few socket types (stream, server, router if flag set) can
  -- return EHOSTUNREACH on send
  CanReturnEHOSTUNREACH "send" = 'True
  CanReturnEHOSTUNREACH _ = 'False

type family CanReturnEINVAL ( function :: Symbol ) :: Bool where
  CanReturnEINVAL "bind" = 'True
  CanReturnEINVAL "connect" = 'True
  CanReturnEINVAL _ = 'False

-- type family CanReturnEMFILE ( function :: Function ) :: Bool where
--   CanReturnEMFILE _ = 'False

type family CanReturnEMTHREAD ( function :: Symbol ) :: Bool where
  CanReturnEMTHREAD "bind" = 'True
  CanReturnEMTHREAD "connect" = 'True
  CanReturnEMTHREAD _ = 'False

type family CanReturnENODEV ( function :: Symbol ) :: Bool where
  CanReturnENODEV "bind" = 'True
  CanReturnENODEV _ = 'False

-- type family CanReturnENOENT ( function :: Function ) :: Bool where
--   CanReturnENOENT _ = 'False

-- type family CanReturnEPROTONOSUPPORT ( function :: Function ) :: Bool where
--   CanReturnEPROTONOSUPPORT _ = 'False


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
