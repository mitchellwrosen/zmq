module Zmq.Internal.Monitor
  ( Event (..),
    monitor,
  )
where

import Control.Exception (throwIO)
import Control.Monad (guard)
import Data.Bits (unsafeShiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Short.Base64.URL qualified as Base64
import Data.Int (Int16, Int32)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Short qualified as Text.Short
import Data.Word (Word8)
import Foreign.C (CInt, Errno)
import Libzmq
import System.Random.Stateful qualified as Random
import Zmq.Error (Error (..), catchingOkErrors, enrichError, throwOkError, unexpectedError)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket qualified as Socket
import Zmq.Pair qualified as Pair

monitor :: Socket.Socket a -> IO (Either Error (IO (Either Error Event)))
monitor Socket.Socket {zsocket, name} = do
  endpointBytes <- Random.uniformShortByteString 16 Random.globalStdGen
  let endpoint = Text.Short.toText ("inproc://" <> Base64.encodeBase64Unpadded endpointBytes)
  catchingOkErrors do
    zhs_socket_monitor zsocket endpoint ZMQ_EVENT_ALL
    -- TODO don't give this a name
    pair <- Pair.open_ (if Text.null name then Options.name (name <> " monitor") else Options.defaultOptions)
    Pair.connect_ pair endpoint
    let receiveEvent :: IO (Either Error Event)
        receiveEvent =
          Pair.receives pair >>= \case
            Left err -> pure (Left err)
            Right message ->
              case parseEvent message of
                Nothing -> receiveEvent
                Just event -> pure (Right event)
    pure receiveEvent

data Event
  = AcceptFailed {-# UNPACK #-} !Errno
  | Accepted {-# UNPACK #-} !Zmq_fd
  | BindFailed {-# UNPACK #-} !Errno
  | CloseFailed {-# UNPACK #-} !Errno
  | Closed {-# UNPACK #-} !Zmq_fd
  | ConnectDelayed
  | ConnectRetried {-# UNPACK #-} !Int
  | Connected {-# UNPACK #-} !Zmq_fd
  | Disconnected {-# UNPACK #-} !Zmq_fd
  | HandshakeFailedAuth {-# UNPACK #-} !Int
  | HandshakeFailedNoDetail
  | HandshakeFailedProtocol {-# UNPACK #-} !Zmq_protocol_error
  | HandshakeSucceeded
  | Listening {-# UNPACK #-} !Zmq_fd
  | MonitorStopped

parseEvent :: [ByteString] -> Maybe Event
parseEvent = \case
  [typeBytes, valueBytes] -> do
    typ <- parseEventType typeBytes
    case typ of
      ZMQ_EVENT_ACCEPTED -> Accepted <$> parseFdEventValue valueBytes
      ZMQ_EVENT_ACCEPT_FAILED -> AcceptFailed <$> parseErrnoEventValue valueBytes
      ZMQ_EVENT_BIND_FAILED -> BindFailed <$> parseErrnoEventValue valueBytes
      ZMQ_EVENT_CLOSED -> Closed <$> parseFdEventValue valueBytes
      ZMQ_EVENT_CLOSE_FAILED -> CloseFailed <$> parseErrnoEventValue valueBytes
      ZMQ_EVENT_CONNECTED -> Connected <$> parseFdEventValue valueBytes
      ZMQ_EVENT_CONNECT_DELAYED -> pure ConnectDelayed
      ZMQ_EVENT_CONNECT_RETRIED -> ConnectRetried <$> parseIntEventValue valueBytes
      ZMQ_EVENT_DISCONNECTED -> Disconnected <$> parseFdEventValue valueBytes
      ZMQ_EVENT_HANDSHAKE_FAILED_AUTH -> HandshakeFailedAuth <$> parseIntEventValue valueBytes
      ZMQ_EVENT_HANDSHAKE_FAILED_NO_DETAIL -> pure HandshakeFailedNoDetail
      ZMQ_EVENT_HANDSHAKE_FAILED_PROTOCOL -> HandshakeFailedProtocol <$> undefined
      ZMQ_EVENT_HANDSHAKE_SUCCEEDED -> pure HandshakeSucceeded
      ZMQ_EVENT_LISTENING -> Listening <$> parseFdEventValue valueBytes
      ZMQ_EVENT_MONITOR_STOPPED -> pure MonitorStopped
      _ -> Nothing
  _ -> Nothing
  where
    parseEventType :: ByteString -> Maybe Zmq_socket_events
    parseEventType bytes = do
      guard (ByteString.length bytes == 2)
      let w0 = fromIntegral @Word8 @Int16 (ByteString.index bytes 0)
      let w1 = fromIntegral @Word8 @Int16 (ByteString.index bytes 1)
      pure (Zmq_socket_events (fromIntegral @Int16 @CInt (unsafeShiftL w0 8 .|. w1)))

    parseEventValue :: ByteString -> Maybe Int32
    parseEventValue bytes = do
      guard (ByteString.length bytes == 4)
      let w0 = fromIntegral @Word8 @Int32 (ByteString.index bytes 0)
      let w1 = fromIntegral @Word8 @Int32 (ByteString.index bytes 1)
      let w2 = fromIntegral @Word8 @Int32 (ByteString.index bytes 2)
      let w3 = fromIntegral @Word8 @Int32 (ByteString.index bytes 3)
      pure (unsafeShiftL w0 24 .|. unsafeShiftL w1 16 .|. unsafeShiftL w2 8 .|. w3)

    parseErrnoEventValue :: ByteString -> Maybe Errno
    parseErrnoEventValue =
      fmap undefined . parseEventValue

    parseFdEventValue :: ByteString -> Maybe Zmq_fd
    parseFdEventValue =
      fmap undefined . parseEventValue

    parseIntEventValue :: ByteString -> Maybe Int
    parseIntEventValue =
      fmap (fromIntegral @Int32 @Int) . parseEventValue

zhs_socket_monitor :: Zmq_socket -> Text -> Zmq_socket_events -> IO ()
zhs_socket_monitor socket endpoint events =
  zmq_socket_monitor socket endpoint events >>= \case
    Left errno ->
      let err = enrichError "zmq_socket_monitor" errno
       in case errno of
            EINVAL -> throwIO err
            ETERM -> throwOkError err
            EPROTONOSUPPORT -> throwIO err
            _ -> unexpectedError err
    Right () -> pure ()
