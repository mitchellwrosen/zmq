module Libzmq.Bindings
  ( -- * Functions

    -- ** Error
    zmq_errno,
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
    zmq_msg_init_data,
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

    -- ** Input/output multiplexing
    zmq_poll,
    zmq_poll__unsafe,

    -- ** Message proxying
    zmq_proxy,
    zmq_proxy_steerable,

    -- ** Probe library capabilities
    zmq_has,

    -- ** Encryption
    zmq_z85_encode,
    zmq_z85_decode,
    zmq_curve_keypair,
    zmq_curve_public,

    -- * Types
    Zmq_fd,
    Zmq_msg,
    Zmq_pollitem (..),

    -- * Constants

    -- ** Errors
    _EADDRINUSE,
    _EADDRNOTAVAIL,
    _EAFNOSUPPORT,
    _ECONNABORTED,
    _ECONNREFUSED,
    _ECONNRESET,
    _EFSM,
    _EHOSTUNREACH,
    _EINPROGRESS,
    _EMSGSIZE,
    _EMTHREAD,
    _ENETDOWN,
    _ENETRESET,
    _ENETUNREACH,
    _ENOBUFS,
    _ENOCOMPATPROTO,
    _ENOTCONN,
    _ENOTSOCK,
    _ENOTSUP,
    _EPROTONOSUPPORT,
    _ETERM,
    _ETIMEDOUT,

    -- ** Context options
    _ZMQ_IO_THREADS,
    _ZMQ_MAX_MSGSZ,
    _ZMQ_MAX_SOCKETS,
    _ZMQ_MSG_T_SIZE,
    _ZMQ_SOCKET_LIMIT,
    _ZMQ_THREAD_AFFINITY_CPU_ADD,
    _ZMQ_THREAD_AFFINITY_CPU_REMOVE,
    _ZMQ_THREAD_NAME_PREFIX,
    _ZMQ_THREAD_PRIORITY,
    _ZMQ_THREAD_SCHED_POLICY,

    -- ** Default for new contexts
    _ZMQ_IO_THREADS_DFLT,
    _ZMQ_MAX_SOCKETS_DFLT,
    _ZMQ_THREAD_PRIORITY_DFLT,
    _ZMQ_THREAD_SCHED_POLICY_DFLT,

    -- ** Socket types
    _ZMQ_DEALER,
    _ZMQ_PAIR,
    _ZMQ_PUB,
    _ZMQ_PULL,
    _ZMQ_PUSH,
    _ZMQ_REP,
    _ZMQ_REQ,
    _ZMQ_ROUTER,
    _ZMQ_STREAM,
    _ZMQ_SUB,
    _ZMQ_XPUB,
    _ZMQ_XSUB,

    -- ** Socket options
    _ZMQ_AFFINITY,
    _ZMQ_BACKLOG,
    _ZMQ_BINDTODEVICE,
    _ZMQ_BLOCKY,
    _ZMQ_CONFLATE,
    _ZMQ_CONNECT_ROUTING_ID,
    _ZMQ_CONNECT_TIMEOUT,
    _ZMQ_CURVE_PUBLICKEY,
    _ZMQ_CURVE_SECRETKEY,
    _ZMQ_CURVE_SERVER,
    _ZMQ_CURVE_SERVERKEY,
    _ZMQ_EVENTS,
    _ZMQ_FD,
    _ZMQ_GSSAPI_PLAINTEXT,
    _ZMQ_GSSAPI_PRINCIPAL,
    _ZMQ_GSSAPI_PRINCIPAL_NAMETYPE,
    _ZMQ_GSSAPI_SERVER,
    _ZMQ_GSSAPI_SERVICE_PRINCIPAL,
    _ZMQ_GSSAPI_SERVICE_PRINCIPAL_NAMETYPE,
    _ZMQ_HANDSHAKE_IVL,
    _ZMQ_HEARTBEAT_IVL,
    _ZMQ_HEARTBEAT_TIMEOUT,
    _ZMQ_HEARTBEAT_TTL,
    _ZMQ_IMMEDIATE,
    _ZMQ_INVERT_MATCHING,
    _ZMQ_IPV6,
    _ZMQ_LAST_ENDPOINT,
    _ZMQ_LINGER,
    _ZMQ_MAXMSGSIZE,
    _ZMQ_MECHANISM,
    _ZMQ_MULTICAST_HOPS,
    _ZMQ_MULTICAST_MAXTPDU,
    _ZMQ_PLAIN_PASSWORD,
    _ZMQ_PLAIN_SERVER,
    _ZMQ_PLAIN_USERNAME,
    _ZMQ_PROBE_ROUTER,
    _ZMQ_RATE,
    _ZMQ_RCVBUF,
    _ZMQ_RCVHWM,
    _ZMQ_RCVMORE,
    _ZMQ_RCVTIMEO,
    _ZMQ_RECONNECT_IVL,
    _ZMQ_RECONNECT_IVL_MAX,
    _ZMQ_RECOVERY_IVL,
    _ZMQ_REQ_CORRELATE,
    _ZMQ_REQ_RELAXED,
    _ZMQ_ROUTER_HANDOVER,
    _ZMQ_ROUTER_MANDATORY,
    _ZMQ_ROUTER_RAW,
    _ZMQ_ROUTING_ID,
    _ZMQ_SNDBUF,
    _ZMQ_SNDHWM,
    _ZMQ_SNDTIMEO,
    _ZMQ_SOCKS_PROXY,
    _ZMQ_STREAM_NOTIFY,
    _ZMQ_SUBSCRIBE,
    _ZMQ_TCP_KEEPALIVE,
    _ZMQ_TCP_KEEPALIVE_CNT,
    _ZMQ_TCP_KEEPALIVE_IDLE,
    _ZMQ_TCP_KEEPALIVE_INTVL,
    _ZMQ_TCP_MAXRT,
    _ZMQ_THREAD_SAFE,
    _ZMQ_TOS,
    _ZMQ_TYPE,
    _ZMQ_UNSUBSCRIBE,
    _ZMQ_USE_FD,
    _ZMQ_VMCI_BUFFER_MAX_SIZE,
    _ZMQ_VMCI_BUFFER_MIN_SIZE,
    _ZMQ_VMCI_BUFFER_SIZE,
    _ZMQ_VMCI_CONNECT_TIMEOUT,
    _ZMQ_XPUB_MANUAL,
    _ZMQ_XPUB_NODROP,
    _ZMQ_XPUB_VERBOSE,
    _ZMQ_XPUB_VERBOSER,
    _ZMQ_XPUB_WELCOME_MSG,
    _ZMQ_ZAP_DOMAIN,

    -- ** Message options
    _ZMQ_MORE,
    _ZMQ_SHARED,

    -- ** Send/recv options
    _ZMQ_DONTWAIT,
    _ZMQ_SNDMORE,

    -- ** Security mechanisms
    _ZMQ_CURVE,
    _ZMQ_GSSAPI,
    _ZMQ_NULL,
    _ZMQ_PLAIN,

    -- ** RADIO-DISH protocol
    _ZMQ_GROUP_MAX_LENGTH,

    -- ** GSSAPI principal name types
    _ZMQ_GSSAPI_NT_HOSTBASED,
    _ZMQ_GSSAPI_NT_KRB5_PRINCIPAL,
    _ZMQ_GSSAPI_NT_USER_NAME,

    -- ** Socket transport events (TCP, IPC and TIPC only)
    _ZMQ_EVENT_ACCEPTED,
    _ZMQ_EVENT_ACCEPT_FAILED,
    _ZMQ_EVENT_ALL,
    _ZMQ_EVENT_BIND_FAILED,
    _ZMQ_EVENT_CLOSED,
    _ZMQ_EVENT_CLOSE_FAILED,
    _ZMQ_EVENT_CONNECTED,
    _ZMQ_EVENT_CONNECT_DELAYED,
    _ZMQ_EVENT_CONNECT_RETRIED,
    _ZMQ_EVENT_DISCONNECTED,
    _ZMQ_EVENT_HANDSHAKE_FAILED_AUTH,
    _ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL,
    _ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL,
    _ZMQ_EVENT_HANDSHAKE_SUCCEEDED,
    _ZMQ_EVENT_LISTENING,
    _ZMQ_EVENT_MONITOR_STOPPED,

    -- ** Protocol errors
    _ZMQ_PROTOCOL_ERROR_WS_UNSPECIFIED,
    _ZMQ_PROTOCOL_ERROR_ZAP_BAD_REQUEST_ID,
    _ZMQ_PROTOCOL_ERROR_ZAP_BAD_VERSION,
    _ZMQ_PROTOCOL_ERROR_ZAP_INVALID_METADATA,
    _ZMQ_PROTOCOL_ERROR_ZAP_INVALID_STATUS_CODE,
    _ZMQ_PROTOCOL_ERROR_ZAP_MALFORMED_REPLY,
    _ZMQ_PROTOCOL_ERROR_ZAP_UNSPECIFIED,
    _ZMQ_PROTOCOL_ERROR_ZMTP_CRYPTOGRAPHIC,
    _ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_METADATA,
    _ZMQ_PROTOCOL_ERROR_ZMTP_INVALID_SEQUENCE,
    _ZMQ_PROTOCOL_ERROR_ZMTP_KEY_EXCHANGE,
    _ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_ERROR,
    _ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_HELLO,
    _ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_INITIATE,
    _ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_MESSAGE,
    _ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_READY,
    _ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_UNSPECIFIED,
    _ZMQ_PROTOCOL_ERROR_ZMTP_MALFORMED_COMMAND_WELCOME,
    _ZMQ_PROTOCOL_ERROR_ZMTP_MECHANISM_MISMATCH,
    _ZMQ_PROTOCOL_ERROR_ZMTP_UNEXPECTED_COMMAND,
    _ZMQ_PROTOCOL_ERROR_ZMTP_UNSPECIFIED,

    -- ** Input/output multiplexing
    _ZMQ_POLLIN,
    _ZMQ_POLLOUT,
    _ZMQ_POLLERR,
    _ZMQ_POLLPRI,

    -- ** Probe library capabilities
    _ZMQ_HAS_CAPABILITIES,
  )
where

import Libzmq.Bindings.Internal.Constants
import Libzmq.Bindings.Internal.Functions
import Libzmq.Bindings.Internal.Types
