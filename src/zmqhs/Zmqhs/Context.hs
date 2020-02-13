module Zmqhs.Context
  ( Context(..)
  ) where

import Foreign.Ptr (Ptr)

import qualified Libzmq


newtype Context
  = Context
  { unContext :: Ptr Libzmq.Context }
  deriving stock ( Eq, Ord, Show )
