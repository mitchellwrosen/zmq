module Zmq.Internal.Context
  ( Context (..),
    globalContextRef,
    run,
  )
where

import Control.Exception
import Data.Foldable (for_)
import Data.IORef
import Libzmq
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafePerformIO)
import Zmq.Error (enrichError, unexpectedError)
import Zmq.Internal.Options (Options (..))
import Zmq.Internal.SocketFinalizer (SocketFinalizer, runSocketFinalizer)

data Context = Context
  { context :: !Zmq_ctx,
    -- A context cannot terminate until all sockets are closed. So, whenever we open a socket, we register a weak
    -- pointer to an action that closes that socket. Sockets thus either close "naturally" (via a finalizer), or during
    -- context termination.
    --
    -- This design allows us to acquire sockets with straight-line syntax, rather than incur a syntactic indent due to
    -- bracketing a resource acquire/release.
    --
    -- FIXME compact this sometimes
    socketFinalizersRef :: !(IORef [SocketFinalizer])
  }

globalContextRef :: IORef Context
globalContextRef =
  unsafePerformIO (newIORef bogusContext)
{-# NOINLINE globalContextRef #-}

bogusContext :: Context
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

newContext :: Options -> IO Context
newContext options = do
  context <- zmq_ctx_new
  setContextOptions context options
  socketFinalizersRef <- newIORef []
  pure Context {context, socketFinalizersRef}

setContextOptions :: Zmq_ctx -> Options -> IO ()
setContextOptions context Options {ioThreads, maxMessageSize, maxSockets} = do
  setContextOption context ZMQ_IO_THREADS (fromIntegral @Natural @Int ioThreads)
  setContextOption context ZMQ_MAX_MSGSZ (fromIntegral @Natural @Int maxMessageSize)
  setContextOption context ZMQ_MAX_SOCKETS (fromIntegral @Natural @Int maxSockets)

-- Terminate a context.
terminateContext :: Context -> IO ()
terminateContext Context {context, socketFinalizersRef} = do
  -- FIXME we should disallow new sockets from being created here
  finalizers <- readIORef socketFinalizersRef
  -- Why reverse: close in the order they were acquired :shrug:
  for_ (reverse finalizers) runSocketFinalizer
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

setContextOption :: Zmq_ctx -> Zmq_ctx_option -> Int -> IO ()
setContextOption context option value =
  zmq_ctx_set context option value >>= \case
    Left errno ->
      let err = enrichError "zmq_ctx_set" errno
       in case errno of
            EINVAL -> throwIO err
            _ -> unexpectedError err
    Right () -> pure ()
