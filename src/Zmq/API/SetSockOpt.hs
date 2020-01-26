module Zmq.API.SetSockOpt
  ( setByteStringSockOpt
  ) where

import qualified Data.ByteString as ByteString
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
    ByteString.unsafeUseAsCString value \c_value ->
      FFI.zmq_setsockopt
        ptr
        key
        c_value
        ( fromIntegral ( ByteString.length value ) )

