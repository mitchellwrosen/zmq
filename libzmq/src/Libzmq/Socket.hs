module Libzmq.Socket
  ( Socket,
    socket,
    close,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    receiveFrame,
    getSocketOption,
    setSocketOption,
  )
where

import Foreign.C
import Foreign.Ptr
import Libzmq.Types (Zmq_ctx)
import Libzmq.Frame (Frame)

data Socket

foreign import ccall unsafe "zmq_socket"
  socket :: Ptr Zmq_ctx -> CInt -> IO (Ptr Socket)

foreign import ccall unsafe "zmq_close"
  close :: Ptr Socket -> IO ()

foreign import ccall safe "zmq_bind"
  bind :: Ptr Socket -> CString -> IO CInt

foreign import ccall safe "zmq_unbind"
  unbind :: Ptr Socket -> CString -> IO CInt

foreign import ccall safe "zmq_connect"
  connect :: Ptr Socket -> CString -> IO CInt

foreign import ccall safe "zmq_disconnect"
  disconnect :: Ptr Socket -> CString -> IO CInt

foreign import ccall safe "zmq_send"
  send :: Ptr Socket -> Ptr a -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "zmq_msg_recv"
  receiveFrame :: Ptr Frame -> Ptr Socket -> CInt -> IO CInt

foreign import ccall unsafe "zmq_getsockopt"
  getSocketOption :: Ptr Socket -> CInt -> Ptr a -> Ptr CSize -> IO CInt

foreign import ccall unsafe "zmq_setsockopt"
  setSocketOption :: Ptr Socket -> CInt -> Ptr a -> CSize -> IO CInt
