module Zmq.Error
  ( Error (..),
    enrichError,
    enrichFunction,
    unexpectedError,
  )
where

import Control.Exception (Exception, throw)
import Data.Functor ((<&>))
import Data.Text (Text)
import Libzmq

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

enrichFunction :: Text -> IO (Either Zmq_error a) -> IO (Either Error a)
enrichFunction function action =
  action <&> \case
    Left errno -> Left (enrichError function errno)
    Right value -> Right value

newtype UnexpectedError
  = UnexpectedError Error
  deriving stock (Show)
  deriving anyclass (Exception)

unexpectedError :: Error -> a
unexpectedError err =
  throw (UnexpectedError err)
