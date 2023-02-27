module Libzmq.Internal.Types (module Libzmq.Internal.Types) where

import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr)
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
newtype Zmq_ctx_t
  = Zmq_ctx_t (Ptr ())
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
    ECONNABORTED -> "ECONNABORTED"
    ECONNREFUSED -> "ECONNREFUSED"
    ECONNRESET -> "ECONNRESET"
    EFSM -> "EFSM"
    EHOSTUNREACH -> "EHOSTUNREACH"
    EINPROGRESS -> "EINPROGRESS"
    EMSGSIZE -> "EMSGSIZE"
    EMTHREAD -> "EMTHREAD"
    ENETDOWN -> "ENETDOWN"
    ENETRESET -> "ENETRESET"
    ENETUNREACH -> "ENETUNREACH"
    ENOBUFS -> "ENOBUFS"
    ENOCOMPATPROTO -> "ENOCOMPATPROTO"
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

-- FIXME are these all the errors possible?

{-# COMPLETE
  EADDRINUSE,
  EADDRNOTAVAIL,
  EAFNOSUPPORT,
  ECONNABORTED,
  ECONNREFUSED,
  ECONNRESET,
  EFSM,
  EHOSTUNREACH,
  EINPROGRESS,
  EMSGSIZE,
  EMTHREAD,
  ENETDOWN,
  ENETRESET,
  ENETUNREACH,
  ENOBUFS,
  ENOCOMPATPROTO,
  ENOTCONN,
  ENOTSOCK,
  ENOTSUP,
  EPROTONOSUPPORT,
  ETERM,
  ETIMEDOUT
  #-}

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
newtype Zmq_msg_t
  = Zmq_msg_t (Ptr Libzmq.Bindings.Zmq_msg_t)
  deriving stock (Eq, Ord, Show)

-- | A ØMQ socket.
data Zmq_socket_t
  = Zmq_socket_t (Ptr ())
  deriving stock (Eq, Ord, Show)

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
