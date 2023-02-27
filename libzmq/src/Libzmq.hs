module Libzmq
  ( -- * Functions

    -- ** Context
    zmq_ctx_new,
    zmq_ctx_term,
    zmq_ctx_shutdown,
    zmq_ctx_set,
    zmq_ctx_get,

    -- ** Message
    zmq_msg_init,
    zmq_msg_init_size,
    -- zmq_msg_init_data,
    zmq_msg_send,
    zmq_msg_send__unsafe,
    zmq_msg_recv,
    zmq_msg_recv__unsafe,
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
    zmq_getsockopt,
    zmq_bind,
    zmq_connect,
    zmq_unbind,
    zmq_disconnect,
    zmq_send,
    zmq_send__unsafe,
    zmq_send_const,
    zmq_send_const__unsafe,
    zmq_recv,
    zmq_recv__unsafe,
    zmq_socket_monitor,

    -- * Types
    Zmq_ctx_option
      ( Zmq_ctx_option,
        ZMQ_BLOCKY,
        ZMQ_IO_THREADS,
        ZMQ_IPV6,
        ZMQ_MAX_MSGSZ,
        ZMQ_MAX_SOCKETS,
        ZMQ_MSG_T_SIZE,
        ZMQ_SOCKET_LIMIT,
        ZMQ_THREAD_NAME_PREFIX,
        ZMQ_THREAD_SCHED_POLICY
      ),
    Zmq_ctx_t,
    Zmq_error
      ( Zmq_error,
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
      ),
    Zmq_msg_t,
    Zmq_socket_t,
  )
where

import Libzmq.Internal.Functions
import Libzmq.Internal.Types
