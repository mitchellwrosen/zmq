module Zmq.Error
  ( Error (..),
    enrichError,
    unexpectedError,
  )
where

import Control.Exception (Exception, throwIO)
import Data.Text (Text)
import Libzmq

-- TODO show args
data Error = Error
  { function :: !Text,
    errno :: !Zmq_error,
    description :: !Text
  }
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

enrichError :: Text -> Zmq_error -> Error
enrichError function errno =
  Error {function, errno, description = zmq_strerror errno}

newtype UnexpectedError
  = UnexpectedError Error
  deriving stock (Show)
  deriving anyclass (Exception)

unexpectedError :: Error -> IO a
unexpectedError err =
  throwIO (UnexpectedError err)
