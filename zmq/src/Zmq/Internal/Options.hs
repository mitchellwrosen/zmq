{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Zmq.Internal.Options
  ( -- * Options
    Options,
    defaultOptions,

    -- ** Context options
    setContextOptions,
    ioThreads,
    maxMessageSize,
    maxSockets,

    -- ** Socket options
    CanSetLossy,
    setSocketOptions,
    setSocketOption,
    lossy,
  )
where

import Control.Exception
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (enrichError, throwOkError, unexpectedError)
import {-# SOURCE #-} Zmq.Internal.Context (Context)

data Options socket
  = DefaultOptions
  | ContextOptions (Zmq_ctx -> IO ())
  | SocketOptions (Zmq_socket -> IO ())

instance Semigroup (Options socket) where
  DefaultOptions <> y = y
  x <> DefaultOptions = x
  ContextOptions x <> ContextOptions y = ContextOptions (x <> y)
  SocketOptions x <> SocketOptions y = SocketOptions (x <> y)
  _ <> _ = DefaultOptions -- type system should prevent this

class CanSetLossy socket

defaultOptions :: Options a
defaultOptions =
  DefaultOptions

------------------------------------------------------------------------------------------------------------------------
-- Context options

setContextOption :: Zmq_ctx -> Zmq_ctx_option -> Int -> IO ()
setContextOption context option value =
  zmq_ctx_set context option value >>= \case
    Left errno ->
      let err = enrichError "zmq_ctx_set" errno
       in case errno of
            EINVAL -> throwIO err
            _ -> unexpectedError err
    Right () -> pure ()

-- Throws ok errors
setContextOptions :: Zmq_ctx -> Options socket -> IO ()
setContextOptions context = \case
  ContextOptions f -> f context
  _ -> pure ()

-- | The number of background IO threads that ØMQ uses.
--
-- As a rule of thumb, each thread can handle 1Gb/sec in or out. If your program performs no external socket IO, you
-- can set this value to 0.
--
-- /Default/: 1
ioThreads :: Natural -> Options Context
ioThreads n =
  ContextOptions \context ->
    setContextOption context ZMQ_IO_THREADS (fromIntegral @Natural @Int n)

maxMessageSize :: Natural -> Options Context
maxMessageSize n =
  ContextOptions \context ->
    setContextOption context ZMQ_MAX_MSGSZ (fromIntegral @Natural @Int n)

maxSockets :: Natural -> Options Context
maxSockets n =
  ContextOptions \context ->
    setContextOption context ZMQ_MAX_SOCKETS (fromIntegral @Natural @Int n)

------------------------------------------------------------------------------------------------------------------------
-- Socket options

-- Throws ok errors
setSocketOption :: Zmq_socket -> Zmq_socket_option a -> a -> IO ()
setSocketOption socket option value = do
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

-- Throws ok errors
setSocketOptions :: Zmq_socket -> Options socket -> IO ()
setSocketOptions socket = \case
  SocketOptions f -> f socket
  _ -> pure ()

lossy :: CanSetLossy socket => Options socket
lossy =
  SocketOptions \socket ->
    setSocketOption socket ZMQ_XPUB_NODROP 1
