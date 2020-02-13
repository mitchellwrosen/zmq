module Libzmq.Context
  ( Context
  , newContext
  ) where

import Foreign.Ptr (Ptr)


data Context

foreign import ccall unsafe "zmq_ctx_new"
  newContext :: IO ( Ptr Context )
