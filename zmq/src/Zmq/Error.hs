-- TODO Zmq.Internal.Error
module Zmq.Error
  ( Error (..),
    enrichError,
    throwOkError,
    catchingOkErrors,
    unexpectedError,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Exception.Base (try)
import Data.Coerce (coerce)
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

-- An error that's actually ok (to be returned to the client as a Left).
-- Used for internal short-circuiting syntax.
newtype OkError
  = OkError Error
  deriving stock (Show)
  deriving anyclass (Exception)

throwOkError :: forall a. Error -> IO a
throwOkError =
  coerce @(OkError -> IO a) throwIO

catchingOkErrors :: forall a. IO a -> IO (Either Error a)
catchingOkErrors action =
  coerce @(IO (Either OkError a)) (try action)

-- Fatal issue: we (the library authors) didn't think this was possible
newtype UnexpectedError
  = UnexpectedError Error
  deriving stock (Show)
  deriving anyclass (Exception)

unexpectedError :: Error -> IO a
unexpectedError err =
  throwIO (UnexpectedError err)
