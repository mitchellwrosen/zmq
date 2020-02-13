module Libzmq.Socket
  ( Socket
  , socket
  , close

  , bind
  , unbind

  , connect
  , disconnect

  , getSocketOption
  ) where

import Foreign.C
import Foreign.Ptr

import Libzmq.Context (Context)


data Socket

foreign import ccall safe "zmq_bind"
  bind :: Ptr Socket -> CString -> IO CInt

foreign import ccall unsafe "zmq_close"
  close :: Ptr Socket -> IO ()

foreign import ccall safe "zmq_connect"
  connect :: Ptr Socket -> CString -> IO CInt

foreign import ccall safe "zmq_disconnect"
  disconnect :: Ptr Socket -> CString -> IO CInt

foreign import ccall unsafe "zmq_socket"
  socket :: Ptr Context -> CInt -> IO ( Ptr Socket )

foreign import ccall safe "zmq_unbind"
  unbind :: Ptr Socket -> CString -> IO CInt

foreign import ccall unsafe "zmq_getsockopt"
  getSocketOption :: Ptr Socket -> CInt -> Ptr a -> Ptr CSize -> IO CInt
