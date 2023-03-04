module Libzmq.Internal.Types (module Libzmq.Internal.Types) where

import Control.Monad (guard)
import Data.Bits ((.&.), (.|.))
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Function ((&))
import Data.Int (Int32, Int64)
import Data.List qualified as List
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Word (Word64)
import Foreign.C.Error
import Foreign.C.Types (CInt, CShort)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import Libzmq.Bindings qualified

-- | A ØMQ context option.
newtype Zmq_ctx_option
  = Zmq_ctx_option CInt
  deriving stock (Eq, Ord)

instance Show Zmq_ctx_option where
  show = \case
    ZMQ_BLOCKY -> "ZMQ_BLOCKY"
    ZMQ_IO_THREADS -> "ZMQ_IO_THREADS"
    ZMQ_IPV6 -> "ZMQ_IPV6"
    ZMQ_MAX_MSGSZ -> "ZMQ_MAX_MSGSZ"
    ZMQ_MAX_SOCKETS -> "ZMQ_MAX_SOCKETS"
    ZMQ_MSG_T_SIZE -> "ZMQ_MSG_T_SIZE"
    ZMQ_SOCKET_LIMIT -> "ZMQ_SOCKET_LIMIT"
    ZMQ_THREAD_NAME_PREFIX -> "ZMQ_THREAD_NAME_PREFIX"
    ZMQ_THREAD_SCHED_POLICY -> "ZMQ_THREAD_SCHED_POLICY"

pattern ZMQ_BLOCKY :: Zmq_ctx_option
pattern ZMQ_BLOCKY <-
  ((== Zmq_ctx_option Libzmq.Bindings._ZMQ_BLOCKY) -> True)
  where
    ZMQ_BLOCKY = Zmq_ctx_option Libzmq.Bindings._ZMQ_BLOCKY

pattern ZMQ_IO_THREADS :: Zmq_ctx_option
pattern ZMQ_IO_THREADS <-
  ((== Zmq_ctx_option Libzmq.Bindings._ZMQ_IO_THREADS) -> True)
  where
    ZMQ_IO_THREADS = Zmq_ctx_option Libzmq.Bindings._ZMQ_IO_THREADS

pattern ZMQ_IPV6 :: Zmq_ctx_option
pattern ZMQ_IPV6 <-
  ((== Zmq_ctx_option Libzmq.Bindings._ZMQ_IPV6) -> True)
  where
    ZMQ_IPV6 = Zmq_ctx_option Libzmq.Bindings._ZMQ_IPV6

pattern ZMQ_MAX_MSGSZ :: Zmq_ctx_option
pattern ZMQ_MAX_MSGSZ <-
  ((== Zmq_ctx_option Libzmq.Bindings._ZMQ_MAX_MSGSZ) -> True)
  where
    ZMQ_MAX_MSGSZ = Zmq_ctx_option Libzmq.Bindings._ZMQ_MAX_MSGSZ

pattern ZMQ_MAX_SOCKETS :: Zmq_ctx_option
pattern ZMQ_MAX_SOCKETS <-
  ((== Zmq_ctx_option Libzmq.Bindings._ZMQ_MAX_SOCKETS) -> True)
  where
    ZMQ_MAX_SOCKETS = Zmq_ctx_option Libzmq.Bindings._ZMQ_MAX_SOCKETS

pattern ZMQ_MSG_T_SIZE :: Zmq_ctx_option
pattern ZMQ_MSG_T_SIZE <-
  ((== Zmq_ctx_option Libzmq.Bindings._ZMQ_MSG_T_SIZE) -> True)
  where
    ZMQ_MSG_T_SIZE = Zmq_ctx_option Libzmq.Bindings._ZMQ_MSG_T_SIZE

pattern ZMQ_SOCKET_LIMIT :: Zmq_ctx_option
pattern ZMQ_SOCKET_LIMIT <-
  ((== Zmq_ctx_option Libzmq.Bindings._ZMQ_SOCKET_LIMIT) -> True)
  where
    ZMQ_SOCKET_LIMIT = Zmq_ctx_option Libzmq.Bindings._ZMQ_SOCKET_LIMIT

pattern ZMQ_THREAD_NAME_PREFIX :: Zmq_ctx_option
pattern ZMQ_THREAD_NAME_PREFIX <-
  ((== Zmq_ctx_option Libzmq.Bindings._ZMQ_THREAD_NAME_PREFIX) -> True)
  where
    ZMQ_THREAD_NAME_PREFIX = Zmq_ctx_option Libzmq.Bindings._ZMQ_THREAD_NAME_PREFIX

pattern ZMQ_THREAD_SCHED_POLICY :: Zmq_ctx_option
pattern ZMQ_THREAD_SCHED_POLICY <-
  ((== Zmq_ctx_option Libzmq.Bindings._ZMQ_THREAD_SCHED_POLICY) -> True)
  where
    ZMQ_THREAD_SCHED_POLICY = Zmq_ctx_option Libzmq.Bindings._ZMQ_THREAD_SCHED_POLICY

{-# COMPLETE
  ZMQ_BLOCKY,
  ZMQ_IO_THREADS,
  ZMQ_IPV6,
  ZMQ_MAX_MSGSZ,
  ZMQ_MAX_SOCKETS,
  ZMQ_MSG_T_SIZE,
  ZMQ_SOCKET_LIMIT,
  ZMQ_THREAD_NAME_PREFIX,
  ZMQ_THREAD_SCHED_POLICY
  #-}

-- | A ØMQ context.
newtype Zmq_ctx
  = Zmq_ctx (Ptr ())
  deriving stock (Eq, Ord, Show)

-- | A ØMQ error.
newtype Zmq_error
  = Zmq_error CInt
  deriving stock (Eq, Ord)

instance Show Zmq_error where
  show = \case
    EADDRINUSE -> "EADDRINUSE"
    EADDRNOTAVAIL -> "EADDRNOTAVAIL"
    EAFNOSUPPORT -> "EAFNOSUPPORT"
    EAGAIN -> "EAGAIN"
    EBADF -> "EBADF"
    ECONNABORTED -> "ECONNABORTED"
    ECONNREFUSED -> "ECONNREFUSED"
    ECONNRESET -> "ECONNRESET"
    EFAULT -> "EFAULT"
    EFSM -> "EFSM"
    EHOSTUNREACH -> "EHOSTUNREACH"
    EINPROGRESS -> "EINPROGRESS"
    EINTR -> "EINTR"
    EINVAL -> "EINVAL"
    EMFILE -> "EMFILE"
    EMSGSIZE -> "EMSGSIZE"
    EMTHREAD -> "EMTHREAD"
    ENETDOWN -> "ENETDOWN"
    ENETRESET -> "ENETRESET"
    ENETUNREACH -> "ENETUNREACH"
    ENOBUFS -> "ENOBUFS"
    ENOCOMPATPROTO -> "ENOCOMPATPROTO"
    ENODEV -> "ENODEV"
    ENOENT -> "ENOENT"
    ENOMEM -> "ENOMEM"
    ENOTCONN -> "ENOTCONN"
    ENOTSOCK -> "ENOTSOCK"
    ENOTSUP -> "ENOTSUP"
    EPROTONOSUPPORT -> "EPROTONOSUPPORT"
    ETERM -> "ETERM"
    ETIMEDOUT -> "ETIMEDOUT"

pattern EADDRINUSE :: Zmq_error
pattern EADDRINUSE <-
  ((== Zmq_error Libzmq.Bindings._EADDRINUSE) -> True)
  where
    EADDRINUSE = Zmq_error Libzmq.Bindings._EADDRINUSE

pattern EADDRNOTAVAIL :: Zmq_error
pattern EADDRNOTAVAIL <-
  ((== Zmq_error Libzmq.Bindings._EADDRNOTAVAIL) -> True)
  where
    EADDRNOTAVAIL = Zmq_error Libzmq.Bindings._EADDRNOTAVAIL

pattern EAFNOSUPPORT :: Zmq_error
pattern EAFNOSUPPORT <-
  ((== Zmq_error Libzmq.Bindings._EAFNOSUPPORT) -> True)
  where
    EAFNOSUPPORT = Zmq_error Libzmq.Bindings._EAFNOSUPPORT

pattern EAGAIN :: Zmq_error
pattern EAGAIN <-
  ((== Zmq_error (coerce @Errno @CInt eAGAIN)) -> True)
  where
    EAGAIN = Zmq_error (coerce @Errno @CInt eAGAIN)

pattern EBADF :: Zmq_error
pattern EBADF <-
  ((== Zmq_error (coerce @Errno @CInt eBADF)) -> True)
  where
    EBADF = Zmq_error (coerce @Errno @CInt eBADF)

pattern ECONNABORTED :: Zmq_error
pattern ECONNABORTED <-
  ((== Zmq_error Libzmq.Bindings._ECONNABORTED) -> True)
  where
    ECONNABORTED = Zmq_error Libzmq.Bindings._ECONNABORTED

pattern ECONNREFUSED :: Zmq_error
pattern ECONNREFUSED <-
  ((== Zmq_error Libzmq.Bindings._ECONNREFUSED) -> True)
  where
    ECONNREFUSED = Zmq_error Libzmq.Bindings._ECONNREFUSED

pattern ECONNRESET :: Zmq_error
pattern ECONNRESET <-
  ((== Zmq_error Libzmq.Bindings._ECONNRESET) -> True)
  where
    ECONNRESET = Zmq_error Libzmq.Bindings._ECONNRESET

pattern EFAULT :: Zmq_error
pattern EFAULT <-
  ((== Zmq_error (coerce @Errno @CInt eFAULT)) -> True)
  where
    EFAULT = Zmq_error (coerce @Errno @CInt eFAULT)

pattern EFSM :: Zmq_error
pattern EFSM <-
  ((== Zmq_error Libzmq.Bindings._EFSM) -> True)
  where
    EFSM = Zmq_error Libzmq.Bindings._EFSM

pattern EHOSTUNREACH :: Zmq_error
pattern EHOSTUNREACH <-
  ((== Zmq_error Libzmq.Bindings._EHOSTUNREACH) -> True)
  where
    EHOSTUNREACH = Zmq_error Libzmq.Bindings._EHOSTUNREACH

pattern EINPROGRESS :: Zmq_error
pattern EINPROGRESS <-
  ((== Zmq_error Libzmq.Bindings._EINPROGRESS) -> True)
  where
    EINPROGRESS = Zmq_error Libzmq.Bindings._EINPROGRESS

pattern EINTR :: Zmq_error
pattern EINTR <-
  ((== Zmq_error (coerce @Errno @CInt eINTR)) -> True)
  where
    EINTR = Zmq_error (coerce @Errno @CInt eINTR)

pattern EINVAL :: Zmq_error
pattern EINVAL <-
  ((== Zmq_error (coerce @Errno @CInt eINVAL)) -> True)
  where
    EINVAL = Zmq_error (coerce @Errno @CInt eINVAL)

pattern EMFILE :: Zmq_error
pattern EMFILE <-
  ((== Zmq_error (coerce @Errno @CInt eMFILE)) -> True)
  where
    EMFILE = Zmq_error (coerce @Errno @CInt eMFILE)

pattern EMSGSIZE :: Zmq_error
pattern EMSGSIZE <-
  ((== Zmq_error Libzmq.Bindings._EMSGSIZE) -> True)
  where
    EMSGSIZE = Zmq_error Libzmq.Bindings._EMSGSIZE

pattern EMTHREAD :: Zmq_error
pattern EMTHREAD <-
  ((== Zmq_error Libzmq.Bindings._EMTHREAD) -> True)
  where
    EMTHREAD = Zmq_error Libzmq.Bindings._EMTHREAD

pattern ENETDOWN :: Zmq_error
pattern ENETDOWN <-
  ((== Zmq_error Libzmq.Bindings._ENETDOWN) -> True)
  where
    ENETDOWN = Zmq_error Libzmq.Bindings._ENETDOWN

pattern ENETRESET :: Zmq_error
pattern ENETRESET <-
  ((== Zmq_error Libzmq.Bindings._ENETRESET) -> True)
  where
    ENETRESET = Zmq_error Libzmq.Bindings._ENETRESET

pattern ENETUNREACH :: Zmq_error
pattern ENETUNREACH <-
  ((== Zmq_error Libzmq.Bindings._ENETUNREACH) -> True)
  where
    ENETUNREACH = Zmq_error Libzmq.Bindings._ENETUNREACH

pattern ENOBUFS :: Zmq_error
pattern ENOBUFS <-
  ((== Zmq_error Libzmq.Bindings._ENOBUFS) -> True)
  where
    ENOBUFS = Zmq_error Libzmq.Bindings._ENOBUFS

pattern ENOCOMPATPROTO :: Zmq_error
pattern ENOCOMPATPROTO <-
  ((== Zmq_error Libzmq.Bindings._ENOCOMPATPROTO) -> True)
  where
    ENOCOMPATPROTO = Zmq_error Libzmq.Bindings._ENOCOMPATPROTO

pattern ENODEV :: Zmq_error
pattern ENODEV <-
  ((== Zmq_error (coerce @Errno @CInt eNODEV)) -> True)
  where
    ENODEV = Zmq_error (coerce @Errno @CInt eNODEV)

pattern ENOENT :: Zmq_error
pattern ENOENT <-
  ((== Zmq_error (coerce @Errno @CInt eNOENT)) -> True)
  where
    ENOENT = Zmq_error (coerce @Errno @CInt eNOENT)

pattern ENOMEM :: Zmq_error
pattern ENOMEM <-
  ((== Zmq_error (coerce @Errno @CInt eNOMEM)) -> True)
  where
    ENOMEM = Zmq_error (coerce @Errno @CInt eNOMEM)

pattern ENOTCONN :: Zmq_error
pattern ENOTCONN <-
  ((== Zmq_error Libzmq.Bindings._ENOTCONN) -> True)
  where
    ENOTCONN = Zmq_error Libzmq.Bindings._ENOTCONN

pattern ENOTSOCK :: Zmq_error
pattern ENOTSOCK <-
  ((== Zmq_error Libzmq.Bindings._ENOTSOCK) -> True)
  where
    ENOTSOCK = Zmq_error Libzmq.Bindings._ENOTSOCK

pattern ENOTSUP :: Zmq_error
pattern ENOTSUP <-
  ((== Zmq_error Libzmq.Bindings._ENOTSUP) -> True)
  where
    ENOTSUP = Zmq_error Libzmq.Bindings._ENOTSUP

pattern EPROTONOSUPPORT :: Zmq_error
pattern EPROTONOSUPPORT <-
  ((== Zmq_error Libzmq.Bindings._EPROTONOSUPPORT) -> True)
  where
    EPROTONOSUPPORT = Zmq_error Libzmq.Bindings._EPROTONOSUPPORT

pattern ETERM :: Zmq_error
pattern ETERM <-
  ((== Zmq_error Libzmq.Bindings._ETERM) -> True)
  where
    ETERM = Zmq_error Libzmq.Bindings._ETERM

pattern ETIMEDOUT :: Zmq_error
pattern ETIMEDOUT <-
  ((== Zmq_error Libzmq.Bindings._ETIMEDOUT) -> True)
  where
    ETIMEDOUT = Zmq_error Libzmq.Bindings._ETIMEDOUT

{-# COMPLETE
  EADDRINUSE,
  EADDRNOTAVAIL,
  EAFNOSUPPORT,
  EAGAIN,
  EBADF,
  ECONNABORTED,
  ECONNREFUSED,
  ECONNRESET,
  EFAULT,
  EFSM,
  EHOSTUNREACH,
  EINPROGRESS,
  EINTR,
  EINVAL,
  EMFILE,
  EMSGSIZE,
  EMTHREAD,
  ENETDOWN,
  ENETRESET,
  ENETUNREACH,
  ENOBUFS,
  ENOCOMPATPROTO,
  ENODEV,
  ENOENT,
  ENOMEM,
  ENOTCONN,
  ENOTSOCK,
  ENOTSUP,
  EPROTONOSUPPORT,
  ETERM,
  ETIMEDOUT
  #-}

-- | A set of ØMQ events.
newtype Zmq_events
  = Zmq_events CShort
  deriving stock (Eq, Ord)

instance Monoid Zmq_events where
  mempty = Zmq_events 0
  mappend = (<>)

instance Semigroup Zmq_events where
  Zmq_events x <> Zmq_events y =
    Zmq_events (x .|. y)

instance Show Zmq_events where
  show event =
    [ "ZMQ_POLLIN" <$ guard (hasPollin event),
      "ZMQ_POLLOUT" <$ guard (hasPollout event),
      "ZMQ_POLLERR" <$ guard (hasPollerr event),
      "ZMQ_POLLPRI" <$ guard (hasPollpri event)
    ]
      & catMaybes
      & List.intersperse "<>"
      & \case
        [] -> "mempty"
        events -> unwords events

pattern ZMQ_POLLIN :: Zmq_events
pattern ZMQ_POLLIN <-
  (hasPollin -> True)
  where
    ZMQ_POLLIN = Zmq_events Libzmq.Bindings._ZMQ_POLLIN

pattern ZMQ_POLLOUT :: Zmq_events
pattern ZMQ_POLLOUT <-
  (hasPollout -> True)
  where
    ZMQ_POLLOUT = Zmq_events Libzmq.Bindings._ZMQ_POLLOUT

pattern ZMQ_POLLERR :: Zmq_events
pattern ZMQ_POLLERR <-
  (hasPollerr -> True)
  where
    ZMQ_POLLERR = Zmq_events Libzmq.Bindings._ZMQ_POLLERR

pattern ZMQ_POLLPRI :: Zmq_events
pattern ZMQ_POLLPRI <-
  (hasPollpri -> True)
  where
    ZMQ_POLLPRI = Zmq_events Libzmq.Bindings._ZMQ_POLLPRI

hasPollin :: Zmq_events -> Bool
hasPollin (Zmq_events n) =
  n .&. Libzmq.Bindings._ZMQ_POLLIN /= 0

hasPollout :: Zmq_events -> Bool
hasPollout (Zmq_events n) =
  n .&. Libzmq.Bindings._ZMQ_POLLOUT /= 0

hasPollerr :: Zmq_events -> Bool
hasPollerr (Zmq_events n) =
  n .&. Libzmq.Bindings._ZMQ_POLLERR /= 0

hasPollpri :: Zmq_events -> Bool
hasPollpri (Zmq_events n) =
  n .&. Libzmq.Bindings._ZMQ_POLLPRI /= 0

-- | A ØMQ message option.
newtype Zmq_msg_option
  = Zmq_msg_option CInt
  deriving stock (Eq, Ord)

instance Show Zmq_msg_option where
  show = \case
    ZMQ_MORE -> "ZMQ_MORE"
    ZMQ_SHARED -> "ZMQ_SHARED"

pattern ZMQ_MORE :: Zmq_msg_option
pattern ZMQ_MORE <-
  ((== Zmq_msg_option Libzmq.Bindings._ZMQ_MORE) -> True)
  where
    ZMQ_MORE = Zmq_msg_option Libzmq.Bindings._ZMQ_MORE

pattern ZMQ_SHARED :: Zmq_msg_option
pattern ZMQ_SHARED <-
  ((== Zmq_msg_option Libzmq.Bindings._ZMQ_SHARED) -> True)
  where
    ZMQ_SHARED = Zmq_msg_option Libzmq.Bindings._ZMQ_SHARED

{-# COMPLETE
  ZMQ_MORE,
  ZMQ_SHARED
  #-}

-- | A ØMQ message.
newtype Zmq_msg
  = Zmq_msg (Ptr Libzmq.Bindings.Zmq_msg)
  deriving stock (Eq, Ord, Show)
  deriving newtype (Storable)

-- | A ØMQ pollitem.
data Zmq_pollitem
  = Zmq_pollitem_socket !Zmq_socket !Zmq_events
  | Zmq_pollitem_fd !Libzmq.Bindings.Zmq_fd !Zmq_events
  deriving stock (Eq, Ord, Show)

-- | A set of ØMQ pollitems.
data Zmq_pollitems
  = Zmq_pollitems !(Ptr Libzmq.Bindings.Zmq_pollitem) !Int
  deriving stock (Eq, Ord, Show)

-- | A ØMQ socket.
data Zmq_socket
  = Zmq_socket (Ptr ())
  deriving stock (Eq, Ord, Show)

-- | A ØMQ socket option.
data Zmq_socket_option a where
  ZMQ_AFFINITY :: Zmq_socket_option Word64
  ZMQ_BACKLOG :: Zmq_socket_option Int32
  ZMQ_BINDTODEVICE :: Zmq_socket_option Text
  ZMQ_CONFLATE :: Zmq_socket_option Int32
  ZMQ_CONNECT_ROUTING_ID :: Zmq_socket_option ByteString
  ZMQ_CONNECT_TIMEOUT :: Zmq_socket_option Int32
  -- ZMQ_CURVE_PUBLICKEY
  -- ZMQ_CURVE_SECRETKEY
  ZMQ_CURVE_SERVER :: Zmq_socket_option Int32
  -- ZMQ_CURVE_SERVERKEY
  ZMQ_GSSAPI_PLAINTEXT :: Zmq_socket_option Int32
  ZMQ_GSSAPI_PRINCIPAL :: Zmq_socket_option Text
  ZMQ_GSSAPI_PRINCIPAL_NAMETYPE :: Zmq_socket_option Int32
  ZMQ_GSSAPI_SERVER :: Zmq_socket_option Int32
  ZMQ_GSSAPI_SERVICE_PRINCIPAL :: Zmq_socket_option Text
  ZMQ_GSSAPI_SERVICE_PRINCIPAL_NAMETYPE :: Zmq_socket_option Int32
  ZMQ_HANDSHAKE_IVL :: Zmq_socket_option Int32
  ZMQ_HEARTBEAT_IVL :: Zmq_socket_option Int32
  ZMQ_HEARTBEAT_TIMEOUT :: Zmq_socket_option Int32
  ZMQ_HEARTBEAT_TTL :: Zmq_socket_option Int32
  ZMQ_IMMEDIATE :: Zmq_socket_option Int32
  ZMQ_INVERT_MATCHING :: Zmq_socket_option Int32
  ZMQ_IPV6' :: Zmq_socket_option Int32
  ZMQ_LINGER :: Zmq_socket_option Int32
  ZMQ_MAXMSGSIZE :: Zmq_socket_option Int64
  ZMQ_MULTICAST_HOPS :: Zmq_socket_option Int32
  ZMQ_MULTICAST_MAXTPDU :: Zmq_socket_option Int32
  ZMQ_PLAIN_PASSWORD :: Zmq_socket_option Text
  ZMQ_PLAIN_SERVER :: Zmq_socket_option Int32
  ZMQ_PLAIN_USERNAME :: Zmq_socket_option Text
  ZMQ_PROBE_ROUTER :: Zmq_socket_option Int32
  ZMQ_RATE :: Zmq_socket_option Int32
  ZMQ_RCVBUF :: Zmq_socket_option Int32
  ZMQ_RCVHWM :: Zmq_socket_option Int32
  ZMQ_RCVTIMEO :: Zmq_socket_option Int32
  ZMQ_RECONNECT_IVL :: Zmq_socket_option Int32
  ZMQ_RECONNECT_IVL_MAX :: Zmq_socket_option Int32
  ZMQ_RECOVERY_IVL :: Zmq_socket_option Int32
  ZMQ_REQ_CORRELATE :: Zmq_socket_option Int32
  ZMQ_REQ_RELAXED :: Zmq_socket_option Int32
  ZMQ_ROUTER_HANDOVER :: Zmq_socket_option Int32
  ZMQ_ROUTER_MANDATORY :: Zmq_socket_option Int32
  ZMQ_ROUTING_ID :: Zmq_socket_option ByteString
  ZMQ_SNDBUF :: Zmq_socket_option Int32
  ZMQ_SNDHWM :: Zmq_socket_option Int32
  ZMQ_SNDTIMEO :: Zmq_socket_option Int32
  ZMQ_SOCKS_PROXY :: Zmq_socket_option Text
  ZMQ_STREAM_NOTIFY :: Zmq_socket_option Int32
  ZMQ_SUBSCRIBE :: Zmq_socket_option ByteString
  ZMQ_TCP_KEEPALIVE :: Zmq_socket_option Int32
  ZMQ_TCP_KEEPALIVE_CNT :: Zmq_socket_option Int32
  ZMQ_TCP_KEEPALIVE_IDLE :: Zmq_socket_option Int32
  ZMQ_TCP_KEEPALIVE_INTVL :: Zmq_socket_option Int32
  ZMQ_TCP_MAXRT :: Zmq_socket_option Int32
  ZMQ_TOS :: Zmq_socket_option Int32
  ZMQ_UNSUBSCRIBE :: Zmq_socket_option ByteString
  ZMQ_USE_FD :: Zmq_socket_option Int32
  ZMQ_VMCI_BUFFER_MAX_SIZE :: Zmq_socket_option Word64
  ZMQ_VMCI_BUFFER_MIN_SIZE :: Zmq_socket_option Word64
  ZMQ_VMCI_BUFFER_SIZE :: Zmq_socket_option Word64
  ZMQ_VMCI_CONNECT_TIMEOUT :: Zmq_socket_option Int32
  ZMQ_XPUB_MANUAL :: Zmq_socket_option Int32
  ZMQ_XPUB_NODROP :: Zmq_socket_option Int32
  ZMQ_XPUB_VERBOSE :: Zmq_socket_option Int32
  ZMQ_XPUB_VERBOSER :: Zmq_socket_option Int32
  ZMQ_XPUB_WELCOME_MSG :: Zmq_socket_option ByteString
  ZMQ_ZAP_DOMAIN :: Zmq_socket_option Text

deriving stock instance Eq (Zmq_socket_option a)

deriving stock instance Show (Zmq_socket_option a)

-- | A ØMQ socket type.
newtype Zmq_socket_type
  = Zmq_socket_type CInt
  deriving stock (Eq, Ord)

instance Show Zmq_socket_type where
  show = \case
    ZMQ_DEALER -> "ZMQ_DEALER"
    ZMQ_PAIR -> "ZMQ_PAIR"
    ZMQ_PUB -> "ZMQ_PUB"
    ZMQ_PULL -> "ZMQ_PULL"
    ZMQ_PUSH -> "ZMQ_PUSH"
    ZMQ_REP -> "ZMQ_REP"
    ZMQ_REQ -> "ZMQ_REQ"
    ZMQ_ROUTER -> "ZMQ_ROUTER"
    ZMQ_STREAM -> "ZMQ_STREAM"
    ZMQ_SUB -> "ZMQ_SUB"
    ZMQ_XPUB -> "ZMQ_XPUB"
    ZMQ_XSUB -> "ZMQ_XSUB"

pattern ZMQ_DEALER :: Zmq_socket_type
pattern ZMQ_DEALER <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_DEALER) -> True)
  where
    ZMQ_DEALER = Zmq_socket_type Libzmq.Bindings._ZMQ_DEALER

pattern ZMQ_PAIR :: Zmq_socket_type
pattern ZMQ_PAIR <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_PAIR) -> True)
  where
    ZMQ_PAIR = Zmq_socket_type Libzmq.Bindings._ZMQ_PAIR

pattern ZMQ_PUB :: Zmq_socket_type
pattern ZMQ_PUB <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_PUB) -> True)
  where
    ZMQ_PUB = Zmq_socket_type Libzmq.Bindings._ZMQ_PUB

pattern ZMQ_PULL :: Zmq_socket_type
pattern ZMQ_PULL <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_PULL) -> True)
  where
    ZMQ_PULL = Zmq_socket_type Libzmq.Bindings._ZMQ_PULL

pattern ZMQ_PUSH :: Zmq_socket_type
pattern ZMQ_PUSH <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_PUSH) -> True)
  where
    ZMQ_PUSH = Zmq_socket_type Libzmq.Bindings._ZMQ_PUSH

pattern ZMQ_REP :: Zmq_socket_type
pattern ZMQ_REP <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_REP) -> True)
  where
    ZMQ_REP = Zmq_socket_type Libzmq.Bindings._ZMQ_REP

pattern ZMQ_REQ :: Zmq_socket_type
pattern ZMQ_REQ <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_REQ) -> True)
  where
    ZMQ_REQ = Zmq_socket_type Libzmq.Bindings._ZMQ_REQ

pattern ZMQ_ROUTER :: Zmq_socket_type
pattern ZMQ_ROUTER <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_ROUTER) -> True)
  where
    ZMQ_ROUTER = Zmq_socket_type Libzmq.Bindings._ZMQ_ROUTER

pattern ZMQ_STREAM :: Zmq_socket_type
pattern ZMQ_STREAM <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_STREAM) -> True)
  where
    ZMQ_STREAM = Zmq_socket_type Libzmq.Bindings._ZMQ_STREAM

pattern ZMQ_SUB :: Zmq_socket_type
pattern ZMQ_SUB <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_SUB) -> True)
  where
    ZMQ_SUB = Zmq_socket_type Libzmq.Bindings._ZMQ_SUB

pattern ZMQ_XPUB :: Zmq_socket_type
pattern ZMQ_XPUB <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_XPUB) -> True)
  where
    ZMQ_XPUB = Zmq_socket_type Libzmq.Bindings._ZMQ_XPUB

pattern ZMQ_XSUB :: Zmq_socket_type
pattern ZMQ_XSUB <-
  ((== Zmq_socket_type Libzmq.Bindings._ZMQ_XSUB) -> True)
  where
    ZMQ_XSUB = Zmq_socket_type Libzmq.Bindings._ZMQ_XSUB

{-# COMPLETE
  ZMQ_DEALER,
  ZMQ_PAIR,
  ZMQ_PUB,
  ZMQ_PULL,
  ZMQ_PUSH,
  ZMQ_REP,
  ZMQ_REQ,
  ZMQ_ROUTER,
  ZMQ_STREAM,
  ZMQ_SUB,
  ZMQ_XPUB,
  ZMQ_XSUB
  #-}
