{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Zmq.Internal.Options
  ( -- * Options
    Options,
    defaultOptions,
    optionsName,

    -- ** Context options
    setContextOption,
    setContextOptions,
    ioThreads,
    maxSockets,

    -- ** Socket options
    CanSetLossy,
    CanSetSendQueueSize,
    setSocketOptions,
    sockopt,
    curveClient,
    curveServer,
    lossy,
    name,
    sendQueueSize,
  )
where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Text qualified as Text
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (enrichError, throwOkError, unexpectedError)
import Zmq.Internal.Curve (CurvePublicKey (..), CurveSecretKey (..), deriveCurvePublicKey)
import {-# SOURCE #-} Zmq.Internal.Socket (Socket)

-- | Options.
data Options a
  = DefaultOptions
  | ContextOptions (Zmq_ctx -> IO ())
  | SocketOptions (Zmq_socket -> IO ()) !Text

type role Options representational

instance Semigroup (Options socket) where
  DefaultOptions <> y = y
  x <> DefaultOptions = x
  ContextOptions x <> ContextOptions y = ContextOptions (x <> y)
  SocketOptions x0 x1 <> SocketOptions y0 y1 = SocketOptions (x0 <> y0) (if Text.null y1 then x1 else y1)
  _ <> _ = DefaultOptions -- type system should prevent this

-- class X socket => CanSetLossy socket
class CanSetLossy socket

-- class X socket => CanSetSendQueueSize socket
class CanSetSendQueueSize socket

-- | Default options.
defaultOptions :: Options a
defaultOptions =
  DefaultOptions

optionsName :: Options socket -> Text
optionsName = \case
  SocketOptions _ s -> s
  _ -> Text.empty

------------------------------------------------------------------------------------------------------------------------
-- Context options

setContextOption :: Zmq_ctx -> Zmq_ctx_option -> Int -> IO ()
setContextOption context option value =
  zmq_ctx_set context option value >>= \case
    Left errno ->
      let err = enrichError "zmq_ctx_set" errno
       in case errno of
            EINVAL -> throwIO err
            _ -> unexpectedError err
    Right () -> pure ()

-- Throws ok errors
setContextOptions :: Zmq_ctx -> Options socket -> IO ()
setContextOptions context = \case
  ContextOptions f -> f context
  _ -> pure ()

-- | The number of background IO threads that ØMQ uses.
--
-- As a rule of thumb, each thread can handle 1Gb/sec of data in or out. If your program performs no external socket IO,
-- you can set this value to 0.
--
-- /Default/: 1
ioThreads :: Natural -> Options ()
ioThreads n =
  ContextOptions \context ->
    setContextOption context ZMQ_IO_THREADS (natToInt n)

-- | The maximum number of sockets that can be open at once, after which @open@ will return 'Zmq.EMFILE'.
--
-- /Default/: 1023
maxSockets :: Natural -> Options ()
maxSockets n =
  ContextOptions \context ->
    setContextOption context ZMQ_MAX_SOCKETS (natToInt (min 1 n)) -- 0 is invalid

------------------------------------------------------------------------------------------------------------------------
-- Socket options

-- Throws ok errors
setSocketOption :: Zmq_socket -> Zmq_socket_option a -> a -> IO ()
setSocketOption socket option value =
  zmq_setsockopt socket option value >>= \case
    Left errno ->
      let err = enrichError "zmq_setsockopt" errno
       in case errno of
            EINTR -> throwOkError err
            EINVAL -> throwIO err
            ENOTSOCK -> throwIO err
            ETERM -> throwOkError err
            _ -> unexpectedError err
    Right val -> pure val

-- Throws ok errors
setSocketOptions :: Zmq_socket -> Options socket -> IO ()
setSocketOptions socket = \case
  SocketOptions f _name -> f socket
  _ -> pure ()

-- | Become a CURVE client.
curveClient :: Socket socket => CurveSecretKey -> CurvePublicKey -> Options socket
curveClient clientSecretKey (CurvePublicKey serverPublicKey) =
  SocketOptions f Text.empty
  where
    CurvePublicKey clientPublicKey =
      deriveCurvePublicKey clientSecretKey

    f :: Zmq_socket -> IO ()
    f socket = do
      setSocketOption socket ZMQ_CURVE_SERVERKEY serverPublicKey
      setSocketOption socket ZMQ_CURVE_PUBLICKEY clientPublicKey
      setSocketOption socket ZMQ_CURVE_SECRETKEY (coerce @CurveSecretKey @ByteString clientSecretKey)

-- | Become a CURVE server.
curveServer :: Socket socket => CurveSecretKey -> Options socket
curveServer (CurveSecretKey secretKey) =
  SocketOptions f Text.empty
  where
    f socket = do
      setSocketOption socket ZMQ_CURVE_SERVER 1
      setSocketOption socket ZMQ_CURVE_SECRETKEY secretKey

lossy :: CanSetLossy socket => Options socket
lossy =
  sockopt ZMQ_XPUB_NODROP 0

name :: Socket socket => Text -> Options socket
name =
  SocketOptions mempty

sendQueueSize :: CanSetSendQueueSize socket => Natural -> Options socket
sendQueueSize n =
  sockopt ZMQ_SNDHWM (natToInt32 n)

-- internal usage only
sockopt :: Zmq_socket_option a -> a -> Options socket
sockopt option value =
  SocketOptions (\socket -> setSocketOption socket option value) Text.empty

------------------------------------------------------------------------------------------------------------------------
-- Utils

natToInt :: Natural -> Int
natToInt n =
  if n > fromIntegral @Int @Natural maxBound
    then maxBound @Int
    else fromIntegral @Natural @Int n

natToInt32 :: Natural -> Int32
natToInt32 n =
  if n > fromIntegral @Int32 @Natural maxBound
    then maxBound @Int32
    else fromIntegral @Natural @Int32 n
