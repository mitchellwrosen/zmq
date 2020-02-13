module Zmq.Exception
  ( Exception(..)
  , exception
  ) where

import Data.Text.Encoding (decodeUtf8)
import qualified Control.Exception
import qualified Data.ByteString.Unsafe as ByteString

import qualified Libzmq

import Zmq.Prelude hiding (Exception)


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
  description <- zmq_texterror errno
  throwIO Exception{..}

zmq_texterror
  :: CInt
  -> IO Text
zmq_texterror =
  fmap decodeUtf8 . ByteString.unsafePackCString . Libzmq.strerror
