module Zmq.Exception
  ( Exception(..)
  , exception
  , Bug(..)
  , unexpectedErrno
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
  description <- zmq_texterror errno
  throwIO Exception{..}


data Bug
  = UnexpectedErrno
  { function :: Text
  , errno :: CInt
  , description :: Text
  } deriving stock ( Eq, Show )
    deriving anyclass ( Control.Exception.Exception )

unexpectedErrno
  :: MonadIO m
  => Text
  -> CInt
  -> m a
unexpectedErrno function errno = liftIO do
  description <- zmq_texterror errno
  throwIO UnexpectedErrno{..}

zmq_texterror
  :: CInt
  -> IO Text
zmq_texterror =
  fmap decodeUtf8 . ByteString.unsafePackCString . FFI.zmq_strerror
