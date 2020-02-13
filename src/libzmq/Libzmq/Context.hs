module Libzmq.Context
  ( Context
  , newContext
  , terminateContext
  ) where

import Foreign.C (CInt(..))
import Foreign.Ptr (Ptr)


data Context

foreign import ccall unsafe "zmq_ctx_new"
  newContext :: IO ( Ptr Context )

foreign import ccall safe "zmq_ctx_term"
  terminateContext :: Ptr Context -> IO CInt
