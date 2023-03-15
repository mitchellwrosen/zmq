{-# LANGUAGE CPP #-}

module Libzmq.Bindings.Internal.Types (module Libzmq.Bindings.Internal.Types) where

#include <zmq.h>

import Data.Coerce (coerce)
import Foreign.C.Types (CChar, CInt, CShort)
#if defined(_WIN32) && !defined(_WIN64)
import Foreign.C.Types (CUInt)
#endif
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))

-- Don't know how to just point at zmq_fd_t, so we copy its definition in here
#if defined _WIN32
#if defined _WIN64
-- | A ØMQ file descriptor.
type Zmq_fd = Word64
#else
-- | A ØMQ file descriptor.
type Zmq_fd = CUInt
#endif
#else
-- | A ØMQ file descriptor.
type Zmq_fd = CInt
#endif

-- | A ØMQ message.
newtype Zmq_msg
  = Zmq_msg (Ptr CChar)
  deriving stock (Eq, Ord, Show)

instance Storable Zmq_msg where
  alignment _ = #{alignment zmq_msg_t}
  sizeOf _ = #{size zmq_msg_t}
  peek = coerce @(Ptr (Ptr CChar) -> IO (Ptr CChar)) #{peek zmq_msg_t, _}
  poke = coerce @(Ptr (Ptr CChar) -> Ptr CChar -> IO ()) #{poke zmq_msg_t, _}

-- | A ØMQ poll item.
data Zmq_pollitem = Zmq_pollitem
  { socket :: !(Ptr ()),
    fd :: !Zmq_fd,
    events :: !CShort,
    revents :: !CShort
  }
  deriving stock (Eq, Ord, Show)

instance Storable Zmq_pollitem where
  alignment _ = #{alignment zmq_pollitem_t}
  sizeOf _ = #{size zmq_pollitem_t}
  peek p =
    Zmq_pollitem
      <$> #{peek zmq_pollitem_t, socket} p
      <*> #{peek zmq_pollitem_t, fd} p
      <*> #{peek zmq_pollitem_t, events} p
      <*> #{peek zmq_pollitem_t, revents} p
  poke p Zmq_pollitem {socket, fd, events, revents} = do
    #{poke zmq_pollitem_t, socket} p socket
    #{poke zmq_pollitem_t, fd} p fd
    #{poke zmq_pollitem_t, events} p events
    #{poke zmq_pollitem_t, revents} p revents
