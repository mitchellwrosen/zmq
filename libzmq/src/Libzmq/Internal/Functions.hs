module Libzmq.Internal.Functions (module Libzmq.Internal.Functions) where

import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Libzmq.Bindings qualified

------------------------------------------------------------------------------------------------------------------------
-- Context

-- | Get a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-get
zmq_ctx_get :: Ptr context -> CInt -> IO CInt
zmq_ctx_get = undefined

-- | Create a new ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-new
zmq_ctx_new :: IO (Ptr context)
zmq_ctx_new = undefined

-- | Set a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-set
zmq_ctx_set :: Ptr context -> CInt -> CInt -> IO CInt
zmq_ctx_set = undefined

-- | Shutdown a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-shutdown
zmq_ctx_shutdown :: Ptr context -> IO CInt
zmq_ctx_shutdown = undefined

-- | Terminate a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-term
zmq_ctx_term :: Ptr context -> IO CInt
zmq_ctx_term = undefined

------------------------------------------------------------------------------------------------------------------------
-- Message

-- | Release a ØMQ message.
--
-- http://api.zeromq.org/master:zmq-msg-close
zmq_msg_close :: Ptr Libzmq.Bindings.Zmq_msg_t -> IO CInt
zmq_msg_close = undefined

-- | Copy the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-copy
zmq_msg_copy :: Ptr Libzmq.Bindings.Zmq_msg_t -> Ptr Libzmq.Bindings.Zmq_msg_t -> IO CInt
zmq_msg_copy = undefined

-- | Get a ØMQ message's content.
--
-- http://api.zeromq.org/master:zmq-msg-data
zmq_msg_data :: Ptr Libzmq.Bindings.Zmq_msg_t -> IO (Ptr a)
zmq_msg_data = undefined

-- | Get a ØMQ message metadata property.
--
-- http://api.zeromq.org/master:zmq-msg-gets
zmq_msg_gets :: Ptr Libzmq.Bindings.Zmq_msg_t -> Ptr CChar -> IO (Ptr CChar)
zmq_msg_gets = undefined

-- | Get a ØMQ message property.
--
-- http://api.zeromq.org/master:zmq-msg-get
zmq_msg_get :: Ptr Libzmq.Bindings.Zmq_msg_t -> CInt -> IO CInt
zmq_msg_get = undefined

-- | Initialise an empty ØMQ message.
--
-- http://api.zeromq.org/master:zmq-msg-init
zmq_msg_init :: Ptr Libzmq.Bindings.Zmq_msg_t -> IO CInt
zmq_msg_init = undefined

-- | Initialize an empty ØMQ message of a specified size.
--
-- http://api.zeromq.org/master:zmq-msg-init-size
zmq_msg_init_size :: Ptr Libzmq.Bindings.Zmq_msg_t -> CSize -> IO CInt
zmq_msg_init_size = undefined

-- | Get whether there are more ØMQ message parts to receive.
--
-- http://api.zeromq.org/master:zmq-msg-more
zmq_msg_more :: Ptr Libzmq.Bindings.Zmq_msg_t -> IO CInt
zmq_msg_more = undefined

-- | Move the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-move
zmq_msg_move :: Ptr Libzmq.Bindings.Zmq_msg_t -> Ptr Libzmq.Bindings.Zmq_msg_t -> IO CInt
zmq_msg_move = undefined

-- | Receive a ØMQ message from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-recv
zmq_msg_recv :: Ptr Libzmq.Bindings.Zmq_msg_t -> Ptr socket -> CInt -> IO CInt
zmq_msg_recv = undefined

-- | Receive a ØMQ message from a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-msg-recv
zmq_msg_recv__unsafe :: Ptr Libzmq.Bindings.Zmq_msg_t -> Ptr socket -> CInt -> IO CInt
zmq_msg_recv__unsafe = undefined

-- | Send a ØMQ message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-send
zmq_msg_send :: Ptr Libzmq.Bindings.Zmq_msg_t -> Ptr socket -> CInt -> IO CInt
zmq_msg_send = undefined

-- | Send a ØMQ message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-msg-send
zmq_msg_send__unsafe :: Ptr Libzmq.Bindings.Zmq_msg_t -> Ptr socket -> CInt -> IO CInt
zmq_msg_send__unsafe = undefined

-- | Set a ØMQ message property.
--
-- http://api.zeromq.org/master:zmq-msg-set
zmq_msg_set :: Ptr Libzmq.Bindings.Zmq_msg_t -> CInt -> CInt -> IO CInt
zmq_msg_set = undefined

-- | Get a ØMQ message's size, in bytes.
--
-- http://api.zeromq.org/master:zmq-msg-size
zmq_msg_size :: Ptr Libzmq.Bindings.Zmq_msg_t -> IO CSize
zmq_msg_size = undefined

------------------------------------------------------------------------------------------------------------------------
-- Socket

-- | Accept incoming connections on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-bind
zmq_bind :: Ptr socket -> Ptr CChar -> IO CInt
zmq_bind = undefined

-- | Close a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-close
zmq_close :: Ptr socket -> IO CInt
zmq_close = undefined

-- | Create an outgoing connection from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-connect
-- N.B. use safe FFI because it's not totally clear if some transports or socket types block
zmq_connect :: Ptr socket -> Ptr CChar -> IO CInt
zmq_connect = undefined

-- | Disconnect a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-disconnect
zmq_disconnect :: Ptr socket -> Ptr CChar -> IO CInt
zmq_disconnect = undefined

-- | Get a ØMQ socket option.
--
-- http://api.zeromq.org/master:zmq-getsockopt
zmq_getsockopt :: Ptr socket -> CInt -> Ptr value -> Ptr CSize -> IO CInt
zmq_getsockopt = undefined

-- | Monitor a ØMQ socket's events.
--
-- http://api.zeromq.org/master:zmq-socket-monitor
zmq_socket_monitor :: Ptr socket -> Ptr CChar -> CInt -> IO CInt
zmq_socket_monitor = undefined

-- | Receive a message from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-recv
zmq_recv :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_recv = undefined

-- | Receive a message from a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-recv
zmq_recv__unsafe :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_recv__unsafe = undefined

-- | Send a message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-send
zmq_send :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_send = undefined

-- | Send a message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-send
zmq_send__unsafe :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_send__unsafe = undefined

-- | Send a constant-memory message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-send-const
zmq_send_const :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_send_const = undefined

-- | Send a constant-memory message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-send-const
zmq_send_const__unsafe :: Ptr socket -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_send_const__unsafe = undefined

-- | Set a ØMQ socket option.
--
-- http://api.zeromq.org/master:zmq-setsockopt
zmq_setsockopt :: Ptr socket -> CInt -> Ptr value -> CSize -> IO CInt
zmq_setsockopt = undefined

-- | Create a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-socket
zmq_socket :: Ptr context -> CInt -> IO (Ptr socket)
zmq_socket = undefined

-- | Stop accepting connections on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-unbind
zmq_unbind :: Ptr socket -> Ptr CChar -> IO CInt
zmq_unbind = undefined
