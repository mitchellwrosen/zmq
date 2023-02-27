module Zmq.Context
  ( newContext,
    Zmqhs.terminateContext,
    withContext,
    Context (..),
    Options (..),
    defaultOptions,
  )
where

import Foreign.C.Types (CInt)
import Libzmq.Bindings qualified as Libzmq
import Numeric.Natural (Natural)
import UnliftIO
import Zmqhs (Context (..))
import Zmqhs qualified

data Options = Options
  { ioThreads :: Natural,
    maxMessageSize :: Natural,
    maxSockets :: Natural
  }

defaultOptions :: Options
defaultOptions =
  Options
    { ioThreads = fromIntegral Libzmq._ZMQ_IO_THREADS_DFLT,
      maxMessageSize = fromIntegral @CInt maxBound,
      maxSockets = fromIntegral Libzmq._ZMQ_MAX_SOCKETS_DFLT
    }

-- | Create a new ZMQ context.
newContext :: MonadIO m => Options -> m Context
newContext options = do
  context <- Zmqhs.newContext
  Zmqhs.setContextIoThreads context (ioThreads options)
  Zmqhs.setContextMaxMessageSize context (maxMessageSize options)
  Zmqhs.setContextMaxSockets context (maxSockets options)
  pure context

withContext :: MonadUnliftIO m => Options -> (Context -> m a) -> m a
withContext options =
  bracket (newContext options) Zmqhs.terminateContext
