module Zmq.Context
  ( withContext,
    Context (..),
    Options (..),
    defaultOptions,
  )
where

import Control.Exception
import Control.Monad.Trans.Except
import Data.Function (fix)
import Data.Functor (void, (<&>))
import Foreign.C.Types (CInt)
import Libzmq qualified
import Libzmq.Bindings qualified as Libzmq.Bindings
import Numeric.Natural (Natural)
import Zmq.Error (Error, enrichError, enrichFunction)

newtype Context
  = Context Libzmq.Zmq_ctx_t

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

-- | Perform an action with a new ØMQ context.
withContext :: Options -> (Context -> IO (Either Error a)) -> IO (Either Error a)
withContext options action =
  mask \restore ->
    newContext options >>= \case
      Left err -> pure (Left err)
      Right context ->
        try (restore (action context)) >>= \case
          Left (exception :: SomeException) -> do
            uninterruptibleMask_ (void (terminateContext context))
            throwIO exception
          Right result -> do
            uninterruptibleMask_ (terminateContext context) <&> \case
              Left err ->
                Left case result of
                  Left err0 -> err0 -- prefer user's error to terminate error
                  Right _ -> err
              Right () -> result

-- Create a new ØMQ context.
newContext :: Options -> IO (Either Error Context)
newContext Options {ioThreads, maxMessageSize, maxSockets} = do
  context <- Libzmq.zmq_ctx_new
  runExceptT do
    zmq_ctx_set context Libzmq.ZMQ_IO_THREADS (fromIntegral @Natural @Int ioThreads)
    zmq_ctx_set context Libzmq.ZMQ_MAX_MSGSZ (fromIntegral @Natural @Int maxMessageSize)
    zmq_ctx_set context Libzmq.ZMQ_MAX_SOCKETS (fromIntegral @Natural @Int maxSockets)
    pure (Context context)
  where
    zmq_ctx_set context option value =
      ExceptT (enrichFunction "zmq_ctx_set" (Libzmq.zmq_ctx_set context option value))

-- Terminate a ØMQ context.
terminateContext :: Context -> IO (Either Error ())
terminateContext (Context context) = do
  fix \again ->
    Libzmq.zmq_ctx_term context >>= \case
      Left Libzmq.EINTR -> again
      Left errno -> pure (Left (enrichError "zmq_ctx_term" errno))
      Right () -> pure (Right ())
