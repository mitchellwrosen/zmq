{-# LANGUAGE UndecidableInstances #-}

module Zmq
  ( BindError
  , ConnectError
  , CanReturnEADDRINUSE
  , CanReturnEADDRNOTAVAIL
  , CanReturnEINVAL
  , CanReturnEMFILE
  , CanReturnEMTHREAD
  , CanReturnENODEV
  , CanReturnEPROTONOSUPPORT
  , Endpoint(..)
  , Error(..)
  , Function(..)
  , Socket
  , SocketError
  , SocketType
  , Transport(..)
  , bind
  , connect
  , socket
  , unbind
  ) where

import Control.Exception (mask)
import Data.Coerce (coerce)
import Data.Kind (Constraint)
import Data.Text (Text)
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as Text
import qualified GHC.TypeLits as TypeLits
import qualified System.ZMQ4.Internal.Base as Zmq


type BindError
  = Error 'Function'Bind

type family CanReturnEADDRINUSE ( function :: Function ) :: Bool where
  CanReturnEADDRINUSE 'Function'Bind = 'True
  CanReturnEADDRINUSE _ = 'False

type family CanReturnEADDRNOTAVAIL ( function :: Function ) :: Bool where
  CanReturnEADDRNOTAVAIL 'Function'Bind = 'True
  CanReturnEADDRNOTAVAIL _ = 'False

type family CanReturnEINVAL ( function :: Function ) :: Bool where
  CanReturnEINVAL 'Function'Bind = 'True
  CanReturnEINVAL 'Function'Connect = 'True
  CanReturnEINVAL 'Function'Unbind = 'True
  CanReturnEINVAL _ = 'False

type family CanReturnEMFILE ( function :: Function ) :: Bool where
  CanReturnEMFILE 'Function'Socket = 'True
  CanReturnEMFILE _ = 'False

type family CanReturnEMTHREAD ( function :: Function ) :: Bool where
  CanReturnEMTHREAD 'Function'Bind = 'True
  CanReturnEMTHREAD 'Function'Connect = 'True
  CanReturnEMTHREAD _ = 'False

type family CanReturnENODEV ( function :: Function ) :: Bool where
  CanReturnENODEV 'Function'Bind = 'True
  CanReturnENODEV _ = 'False

type family CanReturnENOENT ( function :: Function ) :: Bool where
  CanReturnENOENT 'Function'Unbind = 'True
  CanReturnENOENT _ = 'False

type family CanReturnEPROTONOSUPPORT ( function :: Function ) :: Bool where
  CanReturnEPROTONOSUPPORT 'Function'Bind = 'True
  CanReturnEPROTONOSUPPORT 'Function'Connect = 'True
  CanReturnEPROTONOSUPPORT _ = 'False

type family CompatibleTransport ( typ :: SocketType ) ( transport :: Transport ) :: Constraint where
  CompatibleTransport 'Pub  'Transport'Epgm = ()
  CompatibleTransport 'Pub  'Transport'Pgm  = ()
  CompatibleTransport 'Sub  'Transport'Epgm = ()
  CompatibleTransport 'Sub  'Transport'Pgm  = ()
  CompatibleTransport 'XPub 'Transport'Epgm = ()
  CompatibleTransport 'XPub 'Transport'Pgm  = ()
  CompatibleTransport 'XSub 'Transport'Pgm  = ()
  CompatibleTransport 'XSub 'Transport'Epgm = ()

  CompatibleTransport typ 'Transport'Epgm =
    TypeLits.TypeError
      ( 'TypeLits.ShowType typ
        'TypeLits.:<>:
        'TypeLits.Text "is not compatible with the `epgm` transport."
      )
  CompatibleTransport typ 'Transport'Pgm  =
    TypeLits.TypeError
      ( 'TypeLits.ShowType typ
        'TypeLits.:<>:
        'TypeLits.Text "is not compatible with the `pgm` transport."
      )

  CompatibleTransport typ transport = ()

type ConnectError
  = Error 'Function'Connect

data Endpoint ( transport :: Transport ) where
  Epgm   :: Text -> Endpoint 'Transport'Epgm
  Inproc :: Text -> Endpoint 'Transport'Inproc
  Ipc    :: Text -> Endpoint 'Transport'Ipc
  Pgm    :: Text -> Endpoint 'Transport'Pgm
  Tcp    :: Text -> Endpoint 'Transport'Tcp
  Vmci   :: Text -> Endpoint 'Transport'Vmci

data Error ( function :: Function ) where
  EADDRINUSE      :: ( CanReturnEADDRINUSE      function ~ 'True ) => Error function
  EADDRNOTAVAIL   :: ( CanReturnEADDRNOTAVAIL   function ~ 'True ) => Error function
  EINVAL          :: ( CanReturnEINVAL          function ~ 'True ) => Error function
  EMFILE          :: ( CanReturnEMFILE          function ~ 'True ) => Error function
  EMTHREAD        :: ( CanReturnEMTHREAD        function ~ 'True ) => Error function
  ENODEV          :: ( CanReturnENODEV          function ~ 'True ) => Error function
  ENOENT          :: ( CanReturnENOENT          function ~ 'True ) => Error function
  EPROTONOSUPPORT :: ( CanReturnEPROTONOSUPPORT function ~ 'True ) => Error function

instance Show ( Error function ) where
  show = \case
    EADDRINUSE      -> "EADDRINUSE"
    EADDRNOTAVAIL   -> "EADDRNOTAVAIL"
    EINVAL          -> "EINVAL"
    EMFILE          -> "EMFILE"
    EMTHREAD        -> "EMTHREAD"
    ENODEV          -> "ENODEV"
    ENOENT          -> "ENOENT"
    EPROTONOSUPPORT -> "EPROTONOSUPPORT"

data Function
  = Function'Bind
  | Function'Connect
  | Function'Socket
  | Function'Unbind

class IsSocketType ( a :: SocketType ) where
  socketType :: CInt

instance IsSocketType 'Sub where
  socketType =
    coerce Zmq.sub

newtype Socket ( a :: SocketType )
  = Socket
  { unSocket :: ForeignPtr () }

type SocketError
  = Error 'Function'Socket

data SocketType
  = Pub
  | Sub
  | XPub
  | XSub

data Transport
  = Transport'Epgm
  | Transport'Inproc
  | Transport'Ipc
  | Transport'Pgm
  | Transport'Tcp
  | Transport'Vmci

type UnbindError
  = Error 'Function'Unbind

-- | <http://api.zeromq.org/4-3:zmq-bind>
bind
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either BindError () )
bind sock endpoint =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( endpointToString endpoint ) \c_endpoint ->
      Zmq.c_zmq_bind ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          errno >>= \case
            EADDRINUSE_      -> pure ( Left EADDRINUSE )
            EADDRNOTAVAIL_   -> pure ( Left EADDRNOTAVAIL )
            EINVAL_          -> pure ( Left EINVAL )
            EMTHREAD_        -> pure ( Left EMTHREAD )
            ENODEV_          -> pure ( Left ENODEV )
            EPROTONOSUPPORT_ -> pure ( Left EPROTONOSUPPORT )
            -- ENOCOMPATPROTO: CompatibleTransport should prevent it
            -- ENOTSOCK: type system should prevent it
            -- ETERM: global context should prevent it
            n -> unexpectedErrno "bind" n

-- | <http://api.zeromq.org/4-3:zmq-connect>
connect
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either ConnectError () )
connect sock endpoint =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( endpointToString endpoint ) \c_endpoint ->
      Zmq.c_zmq_connect ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          errno >>= \case
            EINVAL_          -> pure ( Left EINVAL )
            EMTHREAD_        -> pure ( Left EMTHREAD )
            EPROTONOSUPPORT_ -> pure ( Left EPROTONOSUPPORT )
            -- ENOCOMPATPROTO: CompatibleTransport should prevent it
            -- ENOTSOCK: type system should prevent it
            -- ETERM: global context should prevent it
            n -> unexpectedErrno "connect" n

-- | Global context.
context :: Zmq.ZMQCtx
context =
  unsafePerformIO ( coerce Zmq.c_zmq_ctx_new )
{-# NOINLINE context #-}

endpointToString
  :: Endpoint transport
  -> String
endpointToString = \case
  Epgm   address -> "epgm://"   ++ Text.unpack address
  Inproc address -> "inproc://" ++ Text.unpack address
  Ipc    address -> "ipc://"    ++ Text.unpack address
  Pgm    address -> "pgm://"    ++ Text.unpack address
  Tcp    address -> "tcp://"    ++ Text.unpack address
  Vmci   address -> "vmci://"   ++ Text.unpack address

errno :: IO Errno
errno =
  coerce Zmq.c_zmq_errno

-- | <http://api.zeromq.org/4-3:zmq-socket>
socket
  :: IsSocketType a
  => IO ( Either SocketError ( Socket a ) )
socket =
  socket_

socket_
  :: forall a.
     IsSocketType a
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

unbind
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either UnbindError () )
unbind sock endpoint =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( endpointToString endpoint ) \c_endpoint ->
      Zmq.c_zmq_unbind ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          errno >>= \case
            EINVAL_ -> pure ( Left EINVAL )
            ENOENT_ -> pure ( Left ENOENT )
            -- ENOTSOCK: type system should prevent it
            -- ETERM: global context should prevent it
            n -> unexpectedErrno "unbind" n

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

pattern ENODEV_ :: Errno
pattern ENODEV_ <- ((== eNODEV) -> True)

pattern ENOENT_ :: Errno
pattern ENOENT_ <- ((== eNOENT) -> True)

pattern EPROTONOSUPPORT_ :: Errno
pattern EPROTONOSUPPORT_ <- ((== coerce Zmq.ePROTONOSUPPORT) -> True)

foreign import ccall unsafe "&zmq_close"
  zmq_close :: FunPtr ( Ptr () -> IO () )
