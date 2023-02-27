module Zmqhs.Context
  ( newContext
  , terminateContext
  , withContext
  , Context(..)

  , setContextIoThreads
  , setContextMaxMessageSize
  , setContextMaxSockets
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce (coerce)
import Data.Function (fix)
import Foreign.C (CInt)
import Foreign.Ptr (Ptr)
import Numeric.Natural (Natural)
import UnliftIO (MonadUnliftIO, bracket)

import qualified Libzmq

import Zmqhs.Internal.Error


newtype Context
  = Context
  { unContext :: Ptr Libzmq.Context }
  deriving stock ( Eq, Ord, Show )

newContext :: MonadIO m => m Context
newContext = liftIO do
  coerce Libzmq.newContext

-- | <http://api.zeromq.org/4-3:zmq-ctx-term>
--
-- May throw:
--   * @EFAULT@ if the provided context was invalid.
terminateContext :: MonadIO m => Context -> m ()
terminateContext context = liftIO do
  fix \again ->
    Libzmq.terminateContext ( unContext context ) >>= \case
      0 -> pure ()
      _ ->
        Libzmq.errno >>= \case
          EINTR -> again
          errno -> throwError "zmq_ctx_term" errno

withContext :: MonadUnliftIO m => ( Context -> m a ) -> m a
withContext =
  bracket newContext terminateContext

-- | <http://api.zeromq.org/4-3:zmq-ctx-set>
setContextIoThreads :: MonadIO m => Context -> Natural -> m ()
setContextIoThreads context n =
  setContextOption context Libzmq.zMQ_IO_THREADS ( fromIntegral n )

-- | <http://api.zeromq.org/4-3:zmq-ctx-set>
setContextMaxMessageSize :: MonadIO m => Context -> Natural -> m ()
setContextMaxMessageSize context n =
  setContextOption
    context
    Libzmq.zMQ_MAX_MSGSZ
    ( fromIntegral ( min n ( fromIntegral ( maxBound :: CInt ) ) ) )

-- | <http://api.zeromq.org/4-3:zmq-ctx-set>
setContextMaxSockets :: MonadIO m => Context -> Natural -> m ()
setContextMaxSockets context n =
  setContextOption context Libzmq.zMQ_MAX_SOCKETS ( fromIntegral n )

setContextOption :: MonadIO m => Context -> CInt -> CInt -> m ()
setContextOption context option value = liftIO do
  Libzmq.setContextOption ( unContext context ) option value >>= \case
    0 -> pure ()
    _ -> Libzmq.errno >>= throwError "zmq_ctx_set"
