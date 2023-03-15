module Zmq.Internal.Context
  ( globalContextRef,
    globalSocketFinalizersRef,
    run,
  )
where

import Control.Exception
import Data.Foldable (for_)
import Data.IORef
import Libzmq
import System.IO.Unsafe (unsafePerformIO)
import Zmq.Error (enrichError, unexpectedError)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.SocketFinalizer (SocketFinalizer, runSocketFinalizer)

-- TODO runtime error if `run` twice

globalContextRef :: IORef Zmq_ctx
globalContextRef =
  unsafePerformIO (newIORef bogusContext)
{-# NOINLINE globalContextRef #-}

-- A context cannot terminate until all sockets are closed. So, whenever we open a socket, we register a weak
-- pointer to an action that closes that socket. Sockets thus either close "naturally" (via a finalizer), or during
-- context termination.
--
-- This design allows us to acquire sockets with straight-line syntax, rather than incur a syntactic indent due to
-- bracketing a resource acquire/release.
--
-- FIXME compact this when a finalizer runs, probably
globalSocketFinalizersRef :: IORef [SocketFinalizer]
globalSocketFinalizersRef =
  unsafePerformIO (newIORef [])
{-# NOINLINE globalSocketFinalizersRef #-}

bogusContext :: Zmq_ctx
bogusContext =
  error "zmq library not initialized"

-- | Run a main function.
--
-- This function must be called exactly once, and must wrap all other calls to this library.
run :: Options () -> IO a -> IO a
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

newContext :: Options () -> IO Zmq_ctx
newContext options = do
  context <- zmq_ctx_new
  Options.setContextOption context ZMQ_BLOCKY 0
  Options.setContextOptions context options
  pure context

-- Terminate a context.
terminateContext :: Zmq_ctx -> IO ()
terminateContext context = do
  -- Shut down the context, causing any blocking operations on sockets to return ETERM
  zmq_ctx_shutdown context >>= \case
    Left errno ->
      let err = enrichError "zmq_ctx_shutdown" errno
       in case errno of
            EFAULT -> throwIO err
            _ -> unexpectedError err
    Right () -> pure ()

  -- Close all of the open sockets
  -- Why reverse: close in the order they were acquired :shrug:
  finalizers <- readIORef globalSocketFinalizersRef
  for_ (reverse finalizers) runSocketFinalizer

  -- Terminate the context
  let loop maybeErr =
        zmq_ctx_term context >>= \case
          Left errno ->
            let err = enrichError "zmq_ctx_term" errno
             in case errno of
                  EFAULT -> throwIO err
                  EINTR -> loop (Just err) -- remember that we were interrupted to throw after termination
                  _ -> unexpectedError err
          Right () -> for_ maybeErr throwIO
  loop Nothing
