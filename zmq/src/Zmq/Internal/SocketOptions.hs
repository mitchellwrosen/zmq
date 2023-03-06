{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Zmq.Internal.SocketOptions
  ( Options,
    defaultOptions,
    setOptions,
    setOption,
    lossy,
  )
where

import Control.Exception
import Libzmq
import Zmq.Error (enrichError, throwOkError, unexpectedError)
import Zmq.Internal.Socket (Socket (getSocket))

newtype Options socket
  = Options (Zmq_socket -> IO ())
  deriving newtype (Semigroup)

class CanSetLossy socket

defaultOptions :: Options socket
defaultOptions =
  Options mempty

-- Throws ok errors
setOptions :: Socket socket => socket -> Options socket -> IO ()
setOptions socket0 (Options f) =
  getSocket socket0 f

-- Throws ok errors
setOption :: Zmq_socket -> Zmq_socket_option a -> a -> IO ()
setOption socket option value = do
  let loop =
        zmq_setsockopt socket option value >>= \case
          Left errno ->
            let err = enrichError "zmq_setsockopt" errno
             in case errno of
                  EINTR -> throwOkError err
                  EINVAL -> throwIO err
                  ENOTSOCK -> throwIO err
                  ETERM -> throwOkError err
                  _ -> unexpectedError err
          Right val -> pure val
  loop

lossy :: CanSetLossy socket => Options socket
lossy =
  Options \socket ->
    setOption socket ZMQ_XPUB_NODROP 1
