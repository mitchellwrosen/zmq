module Zmq.Error
  ( Error (..),
    enrichError,
    enrichFunction,
  )
where

import Control.Exception (Exception)
import Data.Functor ((<&>))
import Data.Text (Text)
import Libzmq qualified

data Error = Error
  { function :: !Text,
    errno :: !Libzmq.Zmq_error,
    description :: !Text
  }
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

enrichError :: Text -> Libzmq.Zmq_error -> Error
enrichError function errno =
  Error {function, errno, description = Libzmq.zmq_strerror errno}

enrichFunction :: Text -> IO (Either Libzmq.Zmq_error a) -> IO (Either Error a)
enrichFunction function action =
  action <&> \case
    Left errno -> Left (enrichError function errno)
    Right value -> Right value
