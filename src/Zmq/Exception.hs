module Zmq.Exception
  ( Exception(..)
  , exception
  ) where

import Data.Text.Encoding (decodeUtf8)
import qualified Control.Exception
import qualified Data.ByteString.Unsafe as ByteString

import Zmq.Prelude
import qualified Zmq.FFI as FFI


data Exception
  = Exception
  { function :: Text
  , errno :: CInt
  , description :: Text
  } deriving stock ( Eq, Show )
    deriving anyclass ( Control.Exception.Exception )

exception
  :: MonadIO m
  => Text
  -> CInt
  -> m a
exception function errno = liftIO do
  description <-
    decodeUtf8 <$> ByteString.unsafePackCString ( FFI.zmq_strerror errno )
  throwIO Exception{..}
