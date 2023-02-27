{-# LANGUAGE CPP #-}

module Libzmq.Bindings.Internal.Types (module Libzmq.Bindings.Internal.Types) where

#include <zmq.h>

import Data.Coerce (coerce)
import Foreign.C.Types (CChar)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))

-- | A ØMQ message.
newtype Zmq_msg_t
  = Zmq_msg_t (Ptr CChar)

instance Storable Zmq_msg_t where
  alignment _ = #{alignment zmq_msg_t}
  sizeOf _ = #{size zmq_msg_t}
  peek = coerce @(Ptr (Ptr CChar) -> IO (Ptr CChar)) #{peek zmq_msg_t, _}
  poke = coerce @(Ptr (Ptr CChar) -> Ptr CChar -> IO ()) #{poke zmq_msg_t, _}
