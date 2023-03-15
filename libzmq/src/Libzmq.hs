{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Libzmq
  ( -- * Functions

    -- ** Error
    zmq_strerror,

    -- ** Version
    zmq_version,

    -- ** Context
    zmq_ctx_new,
    zmq_ctx_term,
    zmq_ctx_shutdown,
    zmq_ctx_set,
    zmq_ctx_get,

    -- ** Message
    zmq_msg_init,
    zmq_msg_init_size,
    zmq_msg_send,
    zmq_msg_send_dontwait,
    zmq_msg_recv,
    zmq_msg_recv_dontwait,
    zmq_msg_close,
    zmq_msg_move,
    zmq_msg_copy,
    zmq_msg_data,
    zmq_msg_size,
    zmq_msg_more,
    zmq_msg_get,
    zmq_msg_set,
    zmq_msg_gets,

    -- ** Socket
    zmq_socket,
    zmq_close,
    zmq_setsockopt,
    zmq_getsockopt_int,
    zmq_getsockopt_word,
    zmq_bind,
    zmq_connect,
    zmq_unbind,
    zmq_disconnect,
    zmq_send,
    zmq_send__unsafe,
    zmq_send_const,
    zmq_send_const__unsafe,
    zmq_recv,
    zmq_recv_dontwait,
    zmq_socket_monitor,

    -- ** Input/output multiplexing
    zmq_poll,

    -- ** Message proxying
    zmq_proxy,
    zmq_proxy_steerable,

    -- ** Probe library capabilities
    zmq_has,

    -- ** Encryption
    zmq_z85_encode,
    zmq_z85_decode,

    -- * Types
    Zmq_ctx_option
      ( ZMQ_BLOCKY,
        ZMQ_IO_THREADS,
        ZMQ_IPV6,
        ZMQ_MAX_MSGSZ,
        ZMQ_MAX_SOCKETS,
        ZMQ_MSG_T_SIZE,
        ZMQ_SOCKET_LIMIT,
        ZMQ_THREAD_NAME_PREFIX,
        ZMQ_THREAD_SCHED_POLICY
      ),
    Zmq_ctx,
    Zmq_error
      ( EADDRINUSE,
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
      ),
    Zmq_events,
    pattern ZMQ_POLLIN,
    pattern ZMQ_POLLOUT,
    pattern ZMQ_POLLERR,
    pattern ZMQ_POLLPRI,
    Libzmq.Bindings.Zmq_fd,
    Zmq_msg_option
      ( ZMQ_MORE,
        ZMQ_SHARED
      ),
    Zmq_msg,
    Libzmq.Bindings.Zmq_pollitem,
    pattern Zmq_pollitem_fd,
    pattern Zmq_pollitem_socket,
    Zmq_send_option,
    pattern ZMQ_DONTWAIT,
    pattern ZMQ_SNDMORE,
    Zmq_socket,
    Zmq_socket_option (..),
    Zmq_socket_type
      ( ZMQ_DEALER,
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
      ),

    -- ** Internals
    Zmq_ctx_option (Zmq_ctx_option),
    Zmq_ctx (Zmq_ctx),
    Zmq_error (Zmq_error),
    Zmq_events (Zmq_events),
    Zmq_msg_option (Zmq_msg_option),
    Zmq_msg (Zmq_msg),
    Libzmq.Bindings.Zmq_pollitem (..),
    Zmq_send_option (Zmq_send_option),
    Zmq_socket (Zmq_socket),
    Zmq_socket_type (Zmq_socket_type),
  )
where

import Libzmq.Bindings qualified
import Libzmq.Internal.Functions
import Libzmq.Internal.Types
