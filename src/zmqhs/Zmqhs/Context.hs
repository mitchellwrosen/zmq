module Zmqhs.Context
  ( Context(..)
  , newContext
  , terminateContext
  , setContextOption
  , ContextOption(..)
  , ioThreads
  , maxSockets
  ) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Coerce (coerce)
import Data.Function (fix)
import Foreign.C (CInt)
import Foreign.Ptr (Ptr)

import qualified Libzmq

import Zmqhs.Error


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
terminateContext
  :: MonadIO m
  => Context
  -> m ()
terminateContext context = liftIO do
  fix \again ->
    Libzmq.terminateContext ( unContext context ) >>= \case
      0 -> pure ()
      _ ->
        Libzmq.errno >>= \case
          EINTR -> again
          errno -> throwError "zmq_ctx_term" errno

setContextOption
  :: MonadIO m
  => Context
  -> ContextOption
  -> CInt
  -> m ( Either CInt () )
setContextOption context option value = liftIO do
  Libzmq.setContextOption ( unContext context ) ( unContextOption option ) value >>= \case
    0 -> pure ( Right () )
    _ -> Left <$> Libzmq.errno


newtype ContextOption
  = ContextOption { unContextOption :: CInt }

ioThreads :: ContextOption
ioThreads =
  ContextOption Libzmq.ioThreads

maxSockets :: ContextOption
maxSockets =
  ContextOption Libzmq.maxSockets
