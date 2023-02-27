module Libzmq.Internal.Functions (module Libzmq.Internal.Functions) where

import Data.Coerce (coerce)
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Libzmq.Bindings qualified
import Libzmq.Internal.Types (Zmq_ctx_option (..), Zmq_ctx_t (..), Zmq_error (..), Zmq_msg_t (..), Zmq_socket_t (..))

-- FIXME place this somewhere
zmq_errno :: IO Zmq_error
zmq_errno =
  coerce Libzmq.Bindings.errno

------------------------------------------------------------------------------------------------------------------------
-- Context

-- | Get a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-get
zmq_ctx_get :: Zmq_ctx_t -> Zmq_ctx_option -> IO (Either Zmq_error Int)
zmq_ctx_get (Zmq_ctx_t context) (Zmq_ctx_option option) = do
  n <- Libzmq.Bindings.zmq_ctx_get context option
  if n >= 0
    then pure (Right (fromIntegral @CInt @Int n))
    else Left <$> zmq_errno

-- | Create a new ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-new
zmq_ctx_new :: IO Zmq_ctx_t
zmq_ctx_new =
  coerce Libzmq.Bindings.zmq_ctx_new

-- | Set a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-set
zmq_ctx_set :: Zmq_ctx_t -> Zmq_ctx_option -> Int -> IO (Either Zmq_error ())
zmq_ctx_set (Zmq_ctx_t context) (Zmq_ctx_option option) value = do
  n <- Libzmq.Bindings.zmq_ctx_set context option (fromIntegral @Int @CInt value)
  if n == 0
    then pure (Right ())
    else Left <$> zmq_errno

-- | Shutdown a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-shutdown
zmq_ctx_shutdown :: Zmq_ctx_t -> IO (Either Zmq_error ())
zmq_ctx_shutdown (Zmq_ctx_t context) = do
  n <- Libzmq.Bindings.zmq_ctx_shutdown context
  if n == 0
    then pure (Right ())
    else Left <$> zmq_errno

-- | Terminate a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-term
zmq_ctx_term :: Zmq_ctx_t -> IO (Either Zmq_error ())
zmq_ctx_term (Zmq_ctx_t context) = do
  n <- Libzmq.Bindings.zmq_ctx_term context
  if n == 0
    then pure (Right ())
    else Left <$> zmq_errno

------------------------------------------------------------------------------------------------------------------------
-- Message

-- | Release a ØMQ message.
--
-- http://api.zeromq.org/master:zmq-msg-close
zmq_msg_close :: Zmq_msg_t -> IO CInt
zmq_msg_close = undefined

-- | Copy the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-copy
zmq_msg_copy :: Zmq_msg_t -> Zmq_msg_t -> IO CInt
zmq_msg_copy = undefined

-- | Get a ØMQ message's content.
--
-- http://api.zeromq.org/master:zmq-msg-data
zmq_msg_data :: Zmq_msg_t -> IO (Ptr a)
zmq_msg_data = undefined

-- | Get a ØMQ message metadata property.
--
-- http://api.zeromq.org/master:zmq-msg-gets
zmq_msg_gets :: Zmq_msg_t -> Ptr CChar -> IO (Ptr CChar)
zmq_msg_gets = undefined

-- | Get a ØMQ message property.
--
-- http://api.zeromq.org/master:zmq-msg-get
zmq_msg_get :: Zmq_msg_t -> CInt -> IO CInt
zmq_msg_get = undefined

-- | Initialise an empty ØMQ message.
--
-- http://api.zeromq.org/master:zmq-msg-init
zmq_msg_init :: Zmq_msg_t -> IO CInt
zmq_msg_init = undefined

-- | Initialize an empty ØMQ message of a specified size.
--
-- http://api.zeromq.org/master:zmq-msg-init-size
zmq_msg_init_size :: Zmq_msg_t -> CSize -> IO CInt
zmq_msg_init_size = undefined

-- | Get whether there are more ØMQ message parts to receive.
--
-- http://api.zeromq.org/master:zmq-msg-more
zmq_msg_more :: Zmq_msg_t -> IO CInt
zmq_msg_more = undefined

-- | Move the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-move
zmq_msg_move :: Zmq_msg_t -> Zmq_msg_t -> IO CInt
zmq_msg_move = undefined

-- | Receive a ØMQ message from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-recv
zmq_msg_recv :: Zmq_msg_t -> Zmq_socket_t -> CInt -> IO CInt
zmq_msg_recv = undefined

-- | Receive a ØMQ message from a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-msg-recv
zmq_msg_recv__unsafe :: Zmq_msg_t -> Zmq_socket_t -> CInt -> IO CInt
zmq_msg_recv__unsafe = undefined

-- | Send a ØMQ message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-send
zmq_msg_send :: Zmq_msg_t -> Zmq_socket_t -> CInt -> IO CInt
zmq_msg_send = undefined

-- | Send a ØMQ message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-msg-send
zmq_msg_send__unsafe :: Zmq_msg_t -> Zmq_socket_t -> CInt -> IO CInt
zmq_msg_send__unsafe = undefined

-- | Set a ØMQ message property.
--
-- http://api.zeromq.org/master:zmq-msg-set
zmq_msg_set :: Zmq_msg_t -> CInt -> CInt -> IO CInt
zmq_msg_set = undefined

-- | Get a ØMQ message's size, in bytes.
--
-- http://api.zeromq.org/master:zmq-msg-size
zmq_msg_size :: Zmq_msg_t -> IO CSize
zmq_msg_size = undefined

------------------------------------------------------------------------------------------------------------------------
-- Socket

-- | Accept incoming connections on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-bind
zmq_bind :: Zmq_socket_t -> Ptr CChar -> IO CInt
zmq_bind = undefined

-- | Close a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-close
zmq_close :: Zmq_socket_t -> IO CInt
zmq_close = undefined

-- | Create an outgoing connection from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-connect
zmq_connect :: Zmq_socket_t -> Ptr CChar -> IO CInt
zmq_connect = undefined

-- | Disconnect a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-disconnect
zmq_disconnect :: Zmq_socket_t -> Ptr CChar -> IO CInt
zmq_disconnect = undefined

-- | Get a ØMQ socket option.
--
-- http://api.zeromq.org/master:zmq-getsockopt
zmq_getsockopt :: Zmq_socket_t -> CInt -> Ptr value -> Ptr CSize -> IO CInt
zmq_getsockopt = undefined

-- | Monitor a ØMQ socket's events.
--
-- http://api.zeromq.org/master:zmq-socket-monitor
zmq_socket_monitor :: Zmq_socket_t -> Ptr CChar -> CInt -> IO CInt
zmq_socket_monitor = undefined

-- | Receive a message from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-recv
zmq_recv :: Zmq_socket_t -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_recv = undefined

-- | Receive a message from a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-recv
zmq_recv__unsafe :: Zmq_socket_t -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_recv__unsafe = undefined

-- | Send a message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-send
zmq_send :: Zmq_socket_t -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_send = undefined

-- | Send a message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-send
zmq_send__unsafe :: Zmq_socket_t -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_send__unsafe = undefined

-- | Send a constant-memory message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-send-const
zmq_send_const :: Zmq_socket_t -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_send_const = undefined

-- | Send a constant-memory message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-send-const
zmq_send_const__unsafe :: Zmq_socket_t -> Ptr buffer -> CSize -> CInt -> IO CInt
zmq_send_const__unsafe = undefined

-- | Set a ØMQ socket option.
--
-- http://api.zeromq.org/master:zmq-setsockopt
zmq_setsockopt :: Zmq_socket_t -> CInt -> Ptr value -> CSize -> IO CInt
zmq_setsockopt = undefined

-- | Create a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-socket
zmq_socket :: Zmq_ctx_t -> CInt -> IO Zmq_socket_t
zmq_socket = undefined

-- | Stop accepting connections on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-unbind
zmq_unbind :: Zmq_socket_t -> Ptr CChar -> IO CInt
zmq_unbind = undefined
