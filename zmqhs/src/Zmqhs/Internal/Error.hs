module Zmqhs.Internal.Error
  ( Error (..),
    throwError,
    pattern EADDRINUSE,
    pattern EADDRNOTAVAIL,
    pattern EAGAIN,
    pattern EFAULT,
    pattern EHOSTUNREACH,
    pattern EINTR,
    pattern EINVAL,
    pattern EMFILE,
    pattern EMTHREAD,
    pattern ENODEV,
    pattern ENOENT,
    pattern ENOTSOCK,
    pattern ETERM,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Unsafe qualified as ByteString
import Data.Coerce (coerce)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Foreign.C
import Libzmq.Bindings qualified as Libzmq

data Error = Error
  { function :: Text,
    errno :: CInt,
    description :: Text
  }
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

throwError ::
  MonadIO m =>
  Text ->
  CInt ->
  m a
throwError function errno = liftIO do
  description <- zmq_texterror errno
  throwIO Error {function, errno, description}

zmq_texterror ::
  CInt ->
  IO Text
zmq_texterror =
  fmap decodeUtf8 . ByteString.unsafePackCString . Libzmq.strerror

pattern EADDRINUSE :: CInt
pattern EADDRINUSE <- ((== Libzmq._EADDRINUSE) -> True)

pattern EADDRNOTAVAIL :: CInt
pattern EADDRNOTAVAIL <- ((== Libzmq._EADDRNOTAVAIL) -> True)

pattern EAGAIN :: CInt
pattern EAGAIN <- ((== coerce eAGAIN) -> True)

pattern EHOSTUNREACH :: CInt
pattern EHOSTUNREACH <- ((== Libzmq._EHOSTUNREACH) -> True)

pattern EINTR :: CInt
pattern EINTR <- ((== coerce eINTR) -> True)

pattern EINVAL :: CInt
pattern EINVAL <-
  ((== coerce eINVAL) -> True)
  where
    EINVAL = coerce eINVAL

pattern EFAULT :: CInt
pattern EFAULT <-
  ((== coerce eFAULT) -> True)
  where
    EFAULT = coerce eFAULT

pattern EMFILE :: CInt
pattern EMFILE <- ((== coerce eMFILE) -> True)

pattern EMTHREAD :: CInt
pattern EMTHREAD <- ((== Libzmq._EMTHREAD) -> True)

pattern ENODEV :: CInt
pattern ENODEV <- ((== coerce eNODEV) -> True)

pattern ENOENT :: CInt
pattern ENOENT <- ((== coerce eNOENT) -> True)

pattern ENOTSOCK :: CInt
pattern ENOTSOCK <-
  ((== Libzmq._ENOTSOCK) -> True)
  where
    ENOTSOCK = Libzmq._ENOTSOCK

pattern ETERM :: CInt
pattern ETERM <-
  ((== Libzmq._ETERM) -> True)
  where
    ETERM = Libzmq._ETERM
