module Libzmq.Bindings.Internal.Error where

import Foreign.C (CInt (..))
import Foreign.C.String (CString)

foreign import ccall unsafe "zmq_errno"
  errno :: IO CInt

foreign import ccall unsafe "zmq_strerror"
  strerror :: CInt -> CString
