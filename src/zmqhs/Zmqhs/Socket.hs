module Zmqhs.Socket
  ( Socket(..)
  ) where

import Foreign.Ptr (Ptr)

import qualified Zmq.FFI as Libzmq


newtype Socket
  = Socket
  { unSocket :: Ptr Libzmq.Socket }
  deriving stock ( Eq, Ord, Show )
