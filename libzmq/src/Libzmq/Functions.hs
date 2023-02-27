module Libzmq.Functions (module Libzmq.Functions) where

import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import Libzmq.Types (Zmq_ctx_t, Zmq_msg_t, Zmq_socket_t)

------------------------------------------------------------------------------------------------------------------------
-- Context

-- | Get a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-get
foreign import capi unsafe "zmq.h zmq_ctx_get"
  zmq_ctx_get :: Ptr Zmq_ctx_t -> CInt -> IO CInt

-- | Create a new ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-new
foreign import capi unsafe "zmq.h zmq_ctx_new"
  zmq_ctx_new :: IO (Ptr Zmq_ctx_t)

-- | Set a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-set
foreign import capi unsafe "zmq.h zmq_ctx_set"
  zmq_ctx_set :: Ptr Zmq_ctx_t -> CInt -> CInt -> IO CInt

-- | Shutdown a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-shutdown
foreign import capi unsafe "zmq.h zmq_ctx_shutdown"
  zmq_ctx_shutdown :: Ptr Zmq_ctx_t -> IO CInt

-- | Terminate a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-term
foreign import capi safe "zmq.h zmq_ctx_term"
  zmq_ctx_term :: Ptr Zmq_ctx_t -> IO CInt

------------------------------------------------------------------------------------------------------------------------
-- Message

-- TODO
-- typedef void(zmq_free_fn) (void *data_, void *hint_);
-- ZMQ_EXPORT int zmq_msg_init_data ( zmq_msg_t *msg_, void *data_, size_t size_, zmq_free_fn *ffn_, void *hint_);

-- | Release a ØMQ message.
--
-- http://api.zeromq.org/master:zmq-msg-close
foreign import capi unsafe "zmq.h zmq_msg_close"
  zmq_msg_close :: Ptr Zmq_msg_t -> IO CInt

-- | Copy the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-copy
foreign import capi unsafe "zmq.h zmq_msg_copy"
  zmq_msg_copy :: Ptr Zmq_msg_t -> Ptr Zmq_msg_t -> IO CInt

-- | Get a ØMQ message's content.
--
-- http://api.zeromq.org/master:zmq-msg-data
foreign import capi unsafe "zmq.h zmq_msg_data"
  zmq_msg_data :: Ptr Zmq_msg_t -> IO (Ptr a)

-- | Get a ØMQ message metadata property.
--
-- http://api.zeromq.org/master:zmq-msg-gets
foreign import capi unsafe "zmq.h zmq_msg_gets"
  zmq_msg_gets :: Ptr Zmq_msg_t -> Ptr CChar -> IO (Ptr CChar)

-- | Get a ØMQ message property.
--
-- http://api.zeromq.org/master:zmq-msg-get
foreign import capi unsafe "zmq.h zmq_msg_get"
  zmq_msg_get :: Ptr Zmq_msg_t -> CInt -> IO CInt

-- | Initialise an empty ØMQ message.
--
-- http://api.zeromq.org/master:zmq-msg-init
foreign import capi unsafe "zmq.h zmq_msg_init"
  zmq_msg_init :: Ptr Zmq_msg_t -> IO CInt

-- | Initialize an empty ØMQ message of a specified size.
--
-- http://api.zeromq.org/master:zmq-msg-init-size
foreign import capi unsafe "zmq.h zmq_msg_init_size"
  zmq_msg_init_size :: Ptr Zmq_msg_t -> CSize -> IO CInt

-- | Get whether there are more ØMQ message parts to receive.
--
-- http://api.zeromq.org/master:zmq-msg-more
foreign import capi unsafe "zmq.h zmq_msg_more"
  zmq_msg_more :: Ptr Zmq_msg_t -> IO CInt

-- | Move the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-move
foreign import capi unsafe "zmq.h zmq_msg_move"
  zmq_msg_move :: Ptr Zmq_msg_t -> Ptr Zmq_msg_t -> IO CInt

-- | Receive a ØMQ message from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-recv
foreign import capi safe "zmq.h zmq_msg_recv"
  zmq_msg_recv :: Ptr Zmq_msg_t -> Ptr Zmq_socket_t -> CInt -> IO CInt

-- | Receive a ØMQ message from a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-msg-recv
foreign import capi unsafe "zmq.h zmq_msg_recv"
  zmq_msg_recv__unsafe :: Ptr Zmq_msg_t -> Ptr Zmq_socket_t -> CInt -> IO CInt

-- | Send a ØMQ message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-send
foreign import capi safe "zmq.h zmq_msg_send"
  zmq_msg_send :: Ptr Zmq_msg_t -> Ptr Zmq_socket_t -> CInt -> IO CInt

-- | Send a ØMQ message on a ØMQ socket (unsafe FFI).
--
-- http://api.zeromq.org/master:zmq-msg-send
foreign import capi unsafe "zmq.h zmq_msg_send"
  zmq_msg_send__unsafe :: Ptr Zmq_msg_t -> Ptr Zmq_socket_t -> CInt -> IO CInt

-- | Set a ØMQ message property.
--
-- http://api.zeromq.org/master:zmq-msg-set
foreign import capi unsafe "zmq.h zmq_msg_set"
  zmq_msg_set :: Ptr Zmq_msg_t -> CInt -> CInt -> IO CInt

-- | Get a ØMQ message's size, in bytes.
--
-- http://api.zeromq.org/master:zmq-msg-size
foreign import capi unsafe "zmq.h zmq_msg_size"
  zmq_msg_size :: Ptr Zmq_msg_t -> IO CSize
