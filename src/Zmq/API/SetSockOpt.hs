module Zmq.API.SetSockOpt
  ( setByteStringSockOpt
  ) where

import qualified Data.ByteString.Unsafe as ByteString

import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


setByteStringSockOpt
  :: Socket a
  -> CInt
  -> ByteString
  -> IO CInt
setByteStringSockOpt socket key value =
  withSocket socket \ptr ->
    ByteString.unsafeUseAsCStringLen value \( c_value, len ) ->
      FFI.zmq_setsockopt
        ptr
        key
        c_value
        ( fromIntegral len )
