module Zmqhs.Socket
  ( Socket(..)
  , unbind
  ) where

import Foreign.C (CInt)
import Foreign.Ptr (Ptr)

import qualified Libzmq

import Zmqhs.Endpoint (Endpoint)


newtype Socket
  = Socket
  { unSocket :: Ptr Libzmq.Socket }
  deriving stock ( Eq, Ord, Show )

unbind
  :: Socket
  -> Endpoint
  -> IO ( Either CInt () )
unbind _socket _endpoint =
  undefined
