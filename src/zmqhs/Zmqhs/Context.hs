module Zmqhs.Context
  ( Context(..)
  , newContext
  , terminateContext
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

terminateContext :: Context -> IO ( Either CInt () )
terminateContext context =
  Libzmq.terminateContext ( unContext context ) >>= \case
    0 -> pure ( Right () )
    _ -> Left <$> FFI.zmq_errno
