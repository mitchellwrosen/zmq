module Libzmq.Functions (module Libzmq.Functions) where

import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Libzmq.Types (Zmq_ctx)

-- | Get a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-get
foreign import capi unsafe "zmq.h zmq_ctx_get"
  zmq_ctx_get :: Ptr Zmq_ctx -> CInt -> IO CInt

-- | Create a new ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-new
foreign import capi unsafe "zmq.h zmq_ctx_new"
  zmq_ctx_new :: IO (Ptr Zmq_ctx)

-- | Set a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-set
foreign import capi unsafe "zmq.h zmq_ctx_set"
  zmq_ctx_set :: Ptr Zmq_ctx -> CInt -> CInt -> IO CInt

-- | Shutdown a ØMQ context
--
-- http://api.zeromq.org/master:zmq-ctx-shutdown
foreign import capi unsafe "zmq.h zmq_ctx_shutdown"
  zmq_ctx_shutdown :: Ptr Zmq_ctx -> IO CInt

-- | Terminate a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-term
foreign import capi safe "zmq.h zmq_ctx_term"
  zmq_ctx_term :: Ptr Zmq_ctx -> IO CInt
