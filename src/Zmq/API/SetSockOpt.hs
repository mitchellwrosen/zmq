module Zmq.API.SetSockOpt
  ( setByteStringSockOpt
  ) where

import qualified Data.ByteString.Unsafe as ByteString

import Zmq.Prelude
import qualified Zmq.FFI as FFI


setByteStringSockOpt
  :: Ptr FFI.Socket
  -> FFI.Sockopt
  -> ByteString
  -> IO CInt
setByteStringSockOpt socket option value =
  ByteString.unsafeUseAsCStringLen value \( c_value, len ) ->
    FFI.zmq_setsockopt socket option c_value ( fromIntegral len )
