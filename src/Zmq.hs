module Zmq
  ( CanReturnEMFILE
  , Error(..)
  , Function(..)
  , Socket
  , SocketError
  , bind
  , socket
    -- * Re-exports
  , Zmq.SocketType
  , Zmq.Sub
  ) where

import Control.Exception (mask)
import Data.Coerce (coerce)
import Data.Text (Text)
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as Text
import qualified System.ZMQ4 as Zmq
import qualified System.ZMQ4.Internal as Zmq
import qualified System.ZMQ4.Internal.Base as Zmq


type family CanReturnEADDRINUSE ( function :: Function ) :: Bool where
  CanReturnEADDRINUSE 'Function'Bind = 'True
  CanReturnEADDRINUSE _ = 'False

type family CanReturnEADDRNOTAVAIL ( function :: Function ) :: Bool where
  CanReturnEADDRNOTAVAIL 'Function'Bind = 'True
  CanReturnEADDRNOTAVAIL _ = 'False

type family CanReturnEINVAL ( function :: Function ) :: Bool where
  CanReturnEINVAL 'Function'Bind = 'True
  CanReturnEINVAL _ = 'False

type family CanReturnEMFILE ( function :: Function ) :: Bool where
  CanReturnEMFILE 'Function'Socket = 'True
  CanReturnEMFILE _ = 'False

type family CanReturnEMTHREAD ( function :: Function ) :: Bool where
  CanReturnEMTHREAD 'Function'Bind = 'True
  CanReturnEMTHREAD _ = 'False

type family CanReturnENOCOMPATPROTO ( function :: Function ) :: Bool where
  CanReturnENOCOMPATPROTO 'Function'Bind = 'True
  CanReturnENOCOMPATPROTO _ = 'False

type family CanReturnENODEV ( function :: Function ) :: Bool where
  CanReturnENODEV 'Function'Bind = 'True
  CanReturnENODEV _ = 'False

type family CanReturnEPROTONOSUPPORT ( function :: Function ) :: Bool where
  CanReturnEPROTONOSUPPORT 'Function'Bind = 'True
  CanReturnEPROTONOSUPPORT _ = 'False

type BindError
  = Error 'Function'Bind

data Error ( function :: Function ) where
  EADDRINUSE      :: ( CanReturnEADDRINUSE      function ~ 'True ) => Error function
  EADDRNOTAVAIL   :: ( CanReturnEADDRNOTAVAIL   function ~ 'True ) => Error function
  EINVAL          :: ( CanReturnEINVAL          function ~ 'True ) => Error function
  EMFILE          :: ( CanReturnEMFILE          function ~ 'True ) => Error function
  EMTHREAD        :: ( CanReturnEMTHREAD        function ~ 'True ) => Error function
  ENOCOMPATPROTO  :: ( CanReturnENOCOMPATPROTO  function ~ 'True ) => Error function
  ENODEV          :: ( CanReturnENODEV          function ~ 'True ) => Error function
  EPROTONOSUPPORT :: ( CanReturnEPROTONOSUPPORT function ~ 'True ) => Error function

instance Show ( Error function ) where
  show = \case
    EADDRINUSE      -> "EADDRINUSE"
    EADDRNOTAVAIL   -> "EADDRNOTAVAIL"
    EINVAL          -> "EINVAL"
    EMFILE          -> "EMFILE"
    EMTHREAD        -> "EMTHREAD"
    ENOCOMPATPROTO  -> "ENOCOMPATPROTO"
    EPROTONOSUPPORT -> "EPROTONOSUPPORT"

data Function
  = Function'Bind
  | Function'Socket

newtype Socket a
  = Socket
  { unSocket :: ForeignPtr () }

type SocketError
  = Error 'Function'Socket

bind
  :: Socket a
  -> Text
  -> IO ( Either BindError () )
bind sock address =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( Text.unpack address ) \c_address ->
      Zmq.c_zmq_bind ptr c_address >>= \case
        0 ->
          pure ( Right () )

        _ ->
          errno >>= \case
            EADDRINUSE_      -> pure ( Left EADDRINUSE )
            EADDRNOTAVAIL_   -> pure ( Left EADDRNOTAVAIL )
            EINVAL_          -> pure ( Left EINVAL )
            EMTHREAD_        -> pure ( Left EMTHREAD )
            ENOCOMPATPROTO_  -> pure ( Left ENOCOMPATPROTO )
            ENODEV_          -> pure ( Left ENODEV )
            EPROTONOSUPPORT_ -> pure ( Left EPROTONOSUPPORT )
            -- ENOTSOCK: type system should prevent it
            -- ETERM: global context should prevent it
            n -> unexpectedErrno "bind" n

-- | Global context.
context :: Zmq.ZMQCtx
context =
  unsafePerformIO ( coerce Zmq.c_zmq_ctx_new )
{-# NOINLINE context #-}

socket
  :: Zmq.SocketType a
  => IO ( Either SocketError ( Socket a ) )
socket =
  socket_

socket_
  :: forall a.
     Zmq.SocketType a
  => IO ( Either SocketError ( Socket a ) )
socket_ =
  mask \unmask -> do
    ptr :: Ptr () <-
      Zmq.c_zmq_socket context ( socketType @a )

    if ptr == nullPtr
      then
        unmask do
          errno >>= \case
            EMFILE_ -> pure ( Left EMFILE )
            -- EFAULT: global context should prevent it
            -- EINVAL: type system should prevent it
            -- ETERM: global context should prevent it
            n -> unexpectedErrno "socket" n

      else do
        foreignPtr :: ForeignPtr () <-
          newForeignPtr zmq_close ptr

        unmask ( pure ( Right ( coerce foreignPtr ) ) )

socketType :: forall a. Zmq.SocketType a => CInt
socketType =
  coerce ( Zmq.zmqSocketType ( undefined :: a ) )

errno :: IO Errno
errno =
  coerce Zmq.c_zmq_errno

unexpectedErrno :: String -> Errno -> a
unexpectedErrno message ( Errno n ) =
  error ( message ++ ": unexpected errno " ++ show n )

pattern EADDRINUSE_ :: Errno
pattern EADDRINUSE_ <- ((== coerce Zmq.eADDRINUSE) -> True)

pattern EADDRNOTAVAIL_ :: Errno
pattern EADDRNOTAVAIL_ <- ((== coerce Zmq.eADDRNOTAVAIL) -> True)

pattern EINVAL_ :: Errno
pattern EINVAL_ <- ((== eINVAL) -> True)

pattern EMFILE_ :: Errno
pattern EMFILE_ <- ((== eMFILE) -> True)

pattern EMTHREAD_ :: Errno
pattern EMTHREAD_ <- ((== coerce Zmq.eMTHREAD) -> True)

pattern ENOCOMPATPROTO_ :: Errno
pattern ENOCOMPATPROTO_ <- ((== coerce Zmq.eNOCOMPATPROTO) -> True)

pattern ENODEV_ :: Errno
pattern ENODEV_ <- ((== eNODEV) -> True)

pattern EPROTONOSUPPORT_ :: Errno
pattern EPROTONOSUPPORT_ <- ((== coerce Zmq.ePROTONOSUPPORT) -> True)

foreign import ccall unsafe "&zmq_close"
  zmq_close :: FunPtr ( Ptr () -> IO () )
