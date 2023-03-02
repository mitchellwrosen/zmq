module Zmqhs.Internal.Error
  ( Error (..),
    throwError,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Unsafe qualified as ByteString
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Foreign.C
import Libzmq qualified as Libzmq

data Error = Error
  { function :: !Text,
    errno :: !Libzmq.Zmq_error,
    description :: !Text
  }
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

throwError :: Text -> Libzmq.Zmq_error -> IO a
throwError function errno =
  throwIO Error {function, errno, description = Libzmq.zmq_strerror errno}
