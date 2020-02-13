module Zmqhs.Context
  ( Context(..)
  , newContext
  , terminateContext
  , setContextOption
  , ContextOption(..)
  , ioThreads
  , maxSockets
  ) where

import Data.Coerce (coerce)
import Foreign.C (CInt)
import Foreign.Ptr (Ptr)

import qualified Libzmq
import qualified Zmq.FFI as FFI


newtype Context
  = Context
  { unContext :: Ptr Libzmq.Context }
  deriving stock ( Eq, Ord, Show )

newContext :: IO Context
newContext =
  coerce Libzmq.newContext

terminateContext
  :: Context
  -> IO ( Either CInt () )
terminateContext context =
  Libzmq.terminateContext ( unContext context ) >>= \case
    0 -> pure ( Right () )
    _ -> Left <$> Libzmq.errno

setContextOption
  :: Context
  -> ContextOption
  -> CInt
  -> IO ( Either CInt () )
setContextOption context option value =
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
