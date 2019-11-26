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
  , Endpoint(..)
  , Error(..)
  , Function(..)
  , Socket
  , SocketError
  , SocketType
  , Transport(..)
  , bind
  , connect
  , disconnect
  , main
  , socket
  , unbind
  ) where

import Control.Exception (bracket_, evaluate, mask)
import Control.Monad.IO.Class
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import qualified System.ZMQ4.Internal.Base as Zmq

import Zmq.Internal


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
  CanReturnEINVAL 'Function'Disconnect = 'True
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

-- type family CanReturnENOENT ( function :: Function ) :: Bool where
--   CanReturnENOENT _ = 'False

-- type family CanReturnEPROTONOSUPPORT ( function :: Function ) :: Bool where
--   CanReturnEPROTONOSUPPORT _ = 'False

type ConnectError
  = Error 'Function'Connect

type DisconnectError
  = Error 'Function'Disconnect

data Error ( function :: Function ) where
  EADDRINUSE      :: ( CanReturnEADDRINUSE      function ~ 'True ) => Error function
  EADDRNOTAVAIL   :: ( CanReturnEADDRNOTAVAIL   function ~ 'True ) => Error function
  EINVAL          :: ( CanReturnEINVAL          function ~ 'True ) => Error function
  EMFILE          :: ( CanReturnEMFILE          function ~ 'True ) => Error function
  EMTHREAD        :: ( CanReturnEMTHREAD        function ~ 'True ) => Error function
  ENODEV          :: ( CanReturnENODEV          function ~ 'True ) => Error function
  -- ENOENT          :: ( CanReturnENOENT          function ~ 'True ) => Error function
  -- EPROTONOSUPPORT :: ( CanReturnEPROTONOSUPPORT function ~ 'True ) => Error function

instance Show ( Error function ) where
  show = \case
    EADDRINUSE      -> "EADDRINUSE"
    EADDRNOTAVAIL   -> "EADDRNOTAVAIL"
    EINVAL          -> "EINVAL"
    EMFILE          -> "EMFILE"
    EMTHREAD        -> "EMTHREAD"
    ENODEV          -> "ENODEV"
    -- ENOENT          -> "ENOENT"
    -- EPROTONOSUPPORT -> "EPROTONOSUPPORT"

data Function
  = Function'Bind
  | Function'Connect
  | Function'Disconnect
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

type UnbindError
  = Error 'Function'Unbind

-- | <http://api.zeromq.org/4-3:zmq-bind>
bind
  :: ( CompatibleTransport typ transport
     , MonadIO m
     )
  => Socket typ
  -> Endpoint transport
  -> m ( Either BindError () )
bind sock endpoint =
  liftIO ( bindIO sock endpoint )

bindIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either BindError () )
bindIO sock endpoint =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( endpointToString endpoint ) \c_endpoint ->
      Zmq.c_zmq_bind ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          errno <&> \case
            EADDRINUSE_    -> Left EADDRINUSE
            EADDRNOTAVAIL_ -> Left EADDRNOTAVAIL
            EINVAL_        -> Left EINVAL
            EMTHREAD_      -> Left EMTHREAD
            ENODEV_        -> Left ENODEV

            ENOTSOCK_      -> Right ()
            ETERM_         -> Right ()

            -- ENOCOMPATPROTO: CompatibleTransport should prevent it
            -- EPROTONOSUPPORT: CPP should prevent it

            n -> errUnexpectedErrno "bind" n

-- | <http://api.zeromq.org/4-3:zmq-connect>
connect
  :: ( CompatibleTransport typ transport
     , MonadIO m
     )
  => Socket typ
  -> Endpoint transport
  -> m ( Either ConnectError () )
connect sock endpoint =
  liftIO ( connectIO sock endpoint )

connectIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either ConnectError () )
connectIO sock endpoint =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( endpointToString endpoint ) \c_endpoint ->
      Zmq.c_zmq_connect ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          errno <&> \case
            EINVAL_   -> Left EINVAL
            EMTHREAD_ -> Left EMTHREAD

            ENOTSOCK_ -> Right ()
            ETERM_    -> Right ()

            -- ENOCOMPATPROTO: CompatibleTransport should prevent it
            -- EPROTONOSUPPORT: CPP should prevent it

            n -> errUnexpectedErrno "connect" n

-- | Global context.
context :: Zmq.ZMQCtx
context =
  unsafePerformIO ( coerce Zmq.c_zmq_ctx_new )
{-# NOINLINE context #-}

-- | <http://api.zeromq.org/4-3:zmq-disconnect>
disconnect
  :: ( CompatibleTransport typ transport
     , MonadIO m
     )
  => Socket typ
  -> Endpoint transport
  -> m ( Either DisconnectError () )
disconnect sock endpoint =
  liftIO ( disconnectIO sock endpoint )

disconnectIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either DisconnectError () )
disconnectIO sock endpoint =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( endpointToString endpoint ) \c_endpoint ->
      Zmq.c_zmq_disconnect ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          errno <&> \case
            EINVAL_   -> Left EINVAL

            ENOENT_   -> Right ()
            ENOTSOCK_ -> Right ()
            ETERM_    -> Right ()

            n -> errUnexpectedErrno "disconnect" n

-- | Run an action in the context of a global ZeroMQ context. This should wrap
-- your @main@ function; functions from this library that are called outside of
-- this context will fail at runtime with 'error'.
main :: IO a -> IO a
main =
  bracket_
    ( evaluate context )
    ( Zmq.c_zmq_ctx_term context )

errno :: IO Errno
errno =
  coerce Zmq.c_zmq_errno

-- | <http://api.zeromq.org/4-3:zmq-socket>
socket
  :: ( IsSocketType a
     , MonadIO m
     )
  => m ( Either SocketError ( Socket a ) )
socket =
  liftIO socketIO

socketIO
  :: forall a.
     IsSocketType a
  => IO ( Either SocketError ( Socket a ) )
socketIO =
  mask \unmask -> do
    ptr :: Ptr () <-
      Zmq.c_zmq_socket context ( socketType @a )

    if ptr == nullPtr
      then
        unmask do
          errno <&> \case
            EMFILE_ -> Left EMFILE

            EFAULT_ -> errInvalidContext
            ETERM_  -> errInvalidContext

            -- EINVAL: type system should prevent it

            n -> errUnexpectedErrno "socket" n

      else do
        foreignPtr :: ForeignPtr () <-
          newForeignPtr zmq_close ptr

        unmask ( pure ( Right ( coerce foreignPtr ) ) )

unbind
  :: ( CompatibleTransport typ transport
     , MonadIO m
     )
  => Socket typ
  -> Endpoint transport
  -> m ( Either UnbindError () )
unbind sock endpoint =
  liftIO ( unbindIO sock endpoint )

unbindIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either UnbindError () )
unbindIO sock endpoint =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( endpointToString endpoint ) \c_endpoint ->
      Zmq.c_zmq_unbind ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          errno <&> \case
            EINVAL_   -> Right ()
            ENOENT_   -> Right ()
            ENOTSOCK_ -> Right ()
            ETERM_    -> Right ()

            n -> errUnexpectedErrno "unbind" n

errInvalidContext :: a
errInvalidContext =
  error "Invalid ZeroMQ context. Did you forget to call 'Zmq.main'?"

errUnexpectedErrno :: String -> Errno -> a
errUnexpectedErrno func ( Errno n ) =
  error ( func ++ ": unexpected errno " ++ show n )

pattern EADDRINUSE_ :: Errno
pattern EADDRINUSE_ <- ((== coerce Zmq.eADDRINUSE) -> True)

pattern EADDRNOTAVAIL_ :: Errno
pattern EADDRNOTAVAIL_ <- ((== coerce Zmq.eADDRNOTAVAIL) -> True)

pattern EINVAL_ :: Errno
pattern EINVAL_ <- ((== eINVAL) -> True)

pattern EFAULT_ :: Errno
pattern EFAULT_ <- ((== eFAULT) -> True)

pattern EMFILE_ :: Errno
pattern EMFILE_ <- ((== eMFILE) -> True)

pattern EMTHREAD_ :: Errno
pattern EMTHREAD_ <- ((== coerce Zmq.eMTHREAD) -> True)

pattern ENODEV_ :: Errno
pattern ENODEV_ <- ((== eNODEV) -> True)

pattern ENOENT_ :: Errno
pattern ENOENT_ <- ((== eNOENT) -> True)

pattern ENOTSOCK_ :: Errno
pattern ENOTSOCK_ <- ((== coerce Zmq.eNOTSOCK) -> True)

pattern ETERM_ :: Errno
pattern ETERM_ <- ((== coerce Zmq.eTERM) -> True)

foreign import ccall unsafe "&zmq_close"
  zmq_close :: FunPtr ( Ptr () -> IO () )
