module Zmq.API.SetSockOpt
  ( setByteStringSockOpt
  , setByteStringSockOpt'
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
  withSocket socket \socket_ptr ->
    ByteString.unsafeUseAsCStringLen value \( c_value, len ) ->
      FFI.zmq_setsockopt socket_ptr key c_value ( fromIntegral len )

setByteStringSockOpt'
  :: ForeignPtr FFI.Socket
  -> CInt
  -> ByteString
  -> IO CInt
setByteStringSockOpt' socket key value =
  withForeignPtr socket \socket_ptr ->
    ByteString.unsafeUseAsCStringLen value \( c_value, len ) ->
      FFI.zmq_setsockopt socket_ptr key c_value ( fromIntegral len )
