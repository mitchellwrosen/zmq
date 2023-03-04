module Zmq.Context
  ( withContext,
    Context (..),
    Options (..),
    defaultOptions,
  )
where

import Control.Exception
import Data.Function (fix)
import Foreign.C.Types (CInt)
import Libzmq
import Libzmq.Bindings qualified as Libzmq.Bindings
import Numeric.Natural (Natural)
import Zmq.Error (Error, enrichError, unexpectedError)

newtype Context
  = Context Zmq_ctx_t

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
  mask \restore -> do
    context <- newContext options
    result <- try (restore (action context))
    uninterruptibleMask_ (terminateContext context)
    case result of
      Left (exception :: SomeException) -> throwIO exception
      Right value -> pure value

-- Create a new ØMQ context.
newContext :: Options -> IO Context
newContext Options {ioThreads, maxMessageSize, maxSockets} = do
  context <- zmq_ctx_new
  setContextOption context ZMQ_IO_THREADS (fromIntegral @Natural @Int ioThreads)
  setContextOption context ZMQ_MAX_MSGSZ (fromIntegral @Natural @Int maxMessageSize)
  setContextOption context ZMQ_MAX_SOCKETS (fromIntegral @Natural @Int maxSockets)
  pure (Context context)

-- Terminate a ØMQ context.
terminateContext :: Context -> IO ()
terminateContext (Context context) = do
  fix \again ->
    zmq_ctx_term context >>= \case
      Left errno ->
        let err = enrichError "zmq_ctx_term" errno
         in case errno of
              EFAULT -> throwIO err
              EINTR -> again
              _ -> unexpectedError err
      Right () -> pure ()

setContextOption :: Zmq_ctx_t -> Zmq_ctx_option -> Int -> IO ()
setContextOption context option value =
  zmq_ctx_set context option value >>= \case
    Left errno ->
      let err = enrichError "zmq_ctx_set" errno
       in case errno of
            EINVAL -> throwIO err
            _ -> unexpectedError err
    Right () -> pure ()
