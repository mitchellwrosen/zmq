module Zmq.Internal.Context
  ( globalContextRef,
    run,
    Options (..),
    defaultOptions,
  )
where

import Control.Exception
import Data.IORef
import Foreign.C.Types (CInt)
import Libzmq
import Libzmq.Bindings qualified as Libzmq.Bindings
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafePerformIO)
import Zmq.Error (enrichError, unexpectedError)

data Options = Options
  { ioThreads :: Natural,
    maxMessageSize :: Natural,
    maxSockets :: Natural
  }

defaultOptions :: Options
defaultOptions =
  Options
    { ioThreads = fromIntegral Libzmq.Bindings._ZMQ_IO_THREADS_DFLT,
      maxMessageSize = fromIntegral @CInt maxBound,
      maxSockets = fromIntegral Libzmq.Bindings._ZMQ_MAX_SOCKETS_DFLT
    }

globalContextRef :: IORef Zmq_ctx_t
globalContextRef =
  unsafePerformIO (newIORef bogusContext)
{-# NOINLINE globalContextRef #-}

bogusContext :: Zmq_ctx_t
bogusContext =
  error "zmq library not initialized"

-- | Run a main function.
--
-- This function must be called exactly once, and must wrap all other calls to this library.
run :: Options -> IO a -> IO a
run options action =
  mask \restore -> do
    context <- newContext options
    writeIORef globalContextRef context
    result <- try (restore action)
    uninterruptibleMask_ (terminateContext context)
    writeIORef globalContextRef bogusContext
    case result of
      Left (exception :: SomeException) -> throwIO exception
      Right value -> pure value

-- Create a new ØMQ context.
newContext :: Options -> IO Zmq_ctx_t
newContext Options {ioThreads, maxMessageSize, maxSockets} = do
  context <- zmq_ctx_new
  setContextOption context ZMQ_IO_THREADS (fromIntegral @Natural @Int ioThreads)
  setContextOption context ZMQ_MAX_MSGSZ (fromIntegral @Natural @Int maxMessageSize)
  setContextOption context ZMQ_MAX_SOCKETS (fromIntegral @Natural @Int maxSockets)
  pure context

-- Terminate a ØMQ context.
terminateContext :: Zmq_ctx_t -> IO ()
terminateContext context = do
  let loop = do
        zmq_ctx_term context >>= \case
          Left errno ->
            let err = enrichError "zmq_ctx_term" errno
             in case errno of
                  EFAULT -> throwIO err
                  EINTR -> loop
                  _ -> unexpectedError err
          Right () -> pure ()
  loop

setContextOption :: Zmq_ctx_t -> Zmq_ctx_option -> Int -> IO ()
setContextOption context option value =
  zmq_ctx_set context option value >>= \case
    Left errno ->
      let err = enrichError "zmq_ctx_set" errno
       in case errno of
            EINVAL -> throwIO err
            _ -> unexpectedError err
    Right () -> pure ()
