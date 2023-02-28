module Libzmq.Internal.Functions (module Libzmq.Internal.Functions) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as ByteString.Unsafe
import Data.Coerce (coerce)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.Foreign qualified as Text
import Data.Word (Word8)
import Foreign (Storable (peek), alloca)
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Libzmq.Bindings qualified
import Libzmq.Internal.Types (Zmq_ctx_option (..), Zmq_ctx_t (..), Zmq_error (..), Zmq_msg_option (..), Zmq_msg_t (..), Zmq_socket_t (..), Zmq_socket_type (..))
import System.IO.Unsafe (unsafeDupablePerformIO)

------------------------------------------------------------------------------------------------------------------------
-- Error

-- | Get the ØMQ error number for the calling thread.
--
-- http://api.zeromq.org/master:zmq-errno
zmq_errno :: IO Zmq_error
zmq_errno =
  coerce Libzmq.Bindings.zmq_errno

-- | Get the string of a ØMQ error number.
--
-- http://api.zeromq.org/master:zmq-strerror
zmq_strerror :: Zmq_error -> Text
zmq_strerror (Zmq_error err) =
  Text.decodeUtf8 (unsafeDupablePerformIO (ByteString.Unsafe.unsafePackCString (Libzmq.Bindings.zmq_strerror err)))

------------------------------------------------------------------------------------------------------------------------
-- Version

-- | The ØMQ library version.
--
-- http://api.zeromq.org/master:zmq-version
zmq_version :: (Int, Int, Int)
zmq_version =
  unsafeDupablePerformIO do
    alloca \px ->
      alloca \py ->
        alloca \pz -> do
          Libzmq.Bindings.zmq_version px py pz
          x <- peek px
          y <- peek py
          z <- peek pz
          pure
            ( fromIntegral @CInt @Int x,
              fromIntegral @CInt @Int y,
              fromIntegral @CInt @Int z
            )

------------------------------------------------------------------------------------------------------------------------
-- Context

-- | Get a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-get
zmq_ctx_get :: Zmq_ctx_t -> Zmq_ctx_option -> IO (Either Zmq_error Int)
zmq_ctx_get (Zmq_ctx_t context) (Zmq_ctx_option option) =
  Libzmq.Bindings.zmq_ctx_get context option >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Create a new ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-new
zmq_ctx_new :: IO Zmq_ctx_t
zmq_ctx_new =
  coerce Libzmq.Bindings.zmq_ctx_new

-- | Set a ØMQ context option.
--
-- http://api.zeromq.org/master:zmq-ctx-set
zmq_ctx_set :: Zmq_ctx_t -> Zmq_ctx_option -> Int -> IO (Either Zmq_error ())
zmq_ctx_set (Zmq_ctx_t context) (Zmq_ctx_option option) value =
  Libzmq.Bindings.zmq_ctx_set context option (fromIntegral @Int @CInt value) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Shutdown a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-shutdown
zmq_ctx_shutdown :: Zmq_ctx_t -> IO (Either Zmq_error ())
zmq_ctx_shutdown (Zmq_ctx_t context) =
  Libzmq.Bindings.zmq_ctx_shutdown context >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Terminate a ØMQ context.
--
-- http://api.zeromq.org/master:zmq-ctx-term
zmq_ctx_term :: Zmq_ctx_t -> IO (Either Zmq_error ())
zmq_ctx_term (Zmq_ctx_t context) =
  Libzmq.Bindings.zmq_ctx_term context >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

------------------------------------------------------------------------------------------------------------------------
-- Message

-- | Release a ØMQ message.
--
-- http://api.zeromq.org/master:zmq-msg-close
zmq_msg_close :: Zmq_msg_t -> IO (Either Zmq_error ())
zmq_msg_close (Zmq_msg_t message) =
  Libzmq.Bindings.zmq_msg_close message >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Copy the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-copy
zmq_msg_copy :: Zmq_msg_t -> Zmq_msg_t -> IO (Either Zmq_error ())
zmq_msg_copy (Zmq_msg_t dst) (Zmq_msg_t src) =
  Libzmq.Bindings.zmq_msg_copy dst src >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Get a ØMQ message's content.
--
-- http://api.zeromq.org/master:zmq-msg-data
zmq_msg_data :: Zmq_msg_t -> IO (Ptr CChar)
zmq_msg_data =
  coerce Libzmq.Bindings.zmq_msg_data

-- | Get a ØMQ message metadata property.
--
-- http://api.zeromq.org/master:zmq-msg-gets
zmq_msg_gets :: Zmq_msg_t -> Text -> IO (Either Zmq_error Text)
zmq_msg_gets (Zmq_msg_t message) property = do
  value <- Text.withCString property (Libzmq.Bindings.zmq_msg_gets message)
  if value == nullPtr then Left <$> zmq_errno else Right <$> Text.fromPtr0 (castPtr value)

-- | Get a ØMQ message option.
--
-- http://api.zeromq.org/master:zmq-msg-get
zmq_msg_get :: Zmq_msg_t -> Zmq_msg_option -> IO (Either Zmq_error Int)
zmq_msg_get (Zmq_msg_t message) (Zmq_msg_option option) =
  Libzmq.Bindings.zmq_msg_get message option >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Initialise an empty ØMQ message.
--
-- http://api.zeromq.org/master:zmq-msg-init
zmq_msg_init :: Zmq_msg_t -> IO ()
zmq_msg_init (Zmq_msg_t message) =
  void (Libzmq.Bindings.zmq_msg_init message) -- always returns 0

-- | Initialize an empty ØMQ message of a specified size.
--
-- http://api.zeromq.org/master:zmq-msg-init-size
zmq_msg_init_size :: Zmq_msg_t -> Int -> IO (Either Zmq_error ())
zmq_msg_init_size (Zmq_msg_t message) size =
  Libzmq.Bindings.zmq_msg_init_size message (fromIntegral @Int @CSize size) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Get whether there are more ØMQ message parts to receive.
--
-- http://api.zeromq.org/master:zmq-msg-more
zmq_msg_more :: Zmq_msg_t -> IO Bool
zmq_msg_more (Zmq_msg_t message) =
  (== 1) <$> Libzmq.Bindings.zmq_msg_more message

-- | Move the content of one ØMQ message to another.
--
-- http://api.zeromq.org/master:zmq-msg-move
zmq_msg_move :: Zmq_msg_t -> Zmq_msg_t -> IO (Either Zmq_error ())
zmq_msg_move (Zmq_msg_t dst) (Zmq_msg_t src) =
  Libzmq.Bindings.zmq_msg_copy dst src >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Receive a ØMQ message from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-recv
zmq_msg_recv :: Zmq_msg_t -> Zmq_socket_t -> IO (Either Zmq_error Int)
zmq_msg_recv (Zmq_msg_t message) (Zmq_socket_t socket) =
  Libzmq.Bindings.zmq_msg_recv message socket 0 >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Receive a ØMQ message from a ØMQ socket (non-blocking).
--
-- http://api.zeromq.org/master:zmq-msg-recv
zmq_msg_recv_dontwait :: Zmq_msg_t -> Zmq_socket_t -> IO (Either Zmq_error Int)
zmq_msg_recv_dontwait (Zmq_msg_t message) (Zmq_socket_t socket) =
  Libzmq.Bindings.zmq_msg_recv__unsafe message socket Libzmq.Bindings._ZMQ_DONTWAIT >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Send a ØMQ message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-msg-send
zmq_msg_send :: Zmq_msg_t -> Zmq_socket_t -> Bool -> IO (Either Zmq_error Int)
zmq_msg_send (Zmq_msg_t message) (Zmq_socket_t socket) more =
  Libzmq.Bindings.zmq_msg_send message socket (if more then Libzmq.Bindings._ZMQ_SNDMORE else 0) >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Send a ØMQ message on a ØMQ socket (non-blocking)
--
-- http://api.zeromq.org/master:zmq-msg-send
zmq_msg_send_dontwait :: Zmq_msg_t -> Zmq_socket_t -> Bool -> IO (Either Zmq_error Int)
zmq_msg_send_dontwait (Zmq_msg_t message) (Zmq_socket_t socket) more =
  Libzmq.Bindings.zmq_msg_send__unsafe message socket flags >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))
  where
    flags =
      if more
        then Libzmq.Bindings._ZMQ_DONTWAIT .|. Libzmq.Bindings._ZMQ_SNDMORE
        else Libzmq.Bindings._ZMQ_DONTWAIT

-- | Set a ØMQ message option.
--
-- http://api.zeromq.org/master:zmq-msg-set
zmq_msg_set :: Zmq_msg_t -> Zmq_msg_option -> Int -> IO (Either Zmq_error ())
zmq_msg_set (Zmq_msg_t message) (Zmq_msg_option option) value =
  Libzmq.Bindings.zmq_msg_set message option (fromIntegral @Int @CInt value) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Get a ØMQ message's size, in bytes.
--
-- http://api.zeromq.org/master:zmq-msg-size
zmq_msg_size :: Zmq_msg_t -> IO Int
zmq_msg_size (Zmq_msg_t message) =
  fromIntegral @CSize @Int <$> Libzmq.Bindings.zmq_msg_size message

------------------------------------------------------------------------------------------------------------------------
-- Socket

-- | Accept incoming connections on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-bind
zmq_bind :: Zmq_socket_t -> Text -> IO (Either Zmq_error ())
zmq_bind (Zmq_socket_t socket) endpoint =
  Text.withCString endpoint (Libzmq.Bindings.zmq_bind socket) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Close a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-close
zmq_close :: Zmq_socket_t -> IO (Either Zmq_error ())
zmq_close (Zmq_socket_t socket) =
  Libzmq.Bindings.zmq_close socket >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Create an outgoing connection from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-connect
zmq_connect :: Zmq_socket_t -> Text -> IO (Either Zmq_error ())
zmq_connect (Zmq_socket_t socket) endpoint =
  Text.withCString endpoint (Libzmq.Bindings.zmq_connect socket) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Disconnect a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-disconnect
zmq_disconnect :: Zmq_socket_t -> Text -> IO (Either Zmq_error ())
zmq_disconnect (Zmq_socket_t socket) endpoint =
  Text.withCString endpoint (Libzmq.Bindings.zmq_connect socket) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno

-- | Get a ØMQ socket option.
--
-- http://api.zeromq.org/master:zmq-getsockopt
--
-- FIXME not implemented
zmq_getsockopt :: Zmq_socket_t -> CInt -> Ptr value -> Ptr CSize -> IO CInt
zmq_getsockopt = undefined

-- | Monitor a ØMQ socket's events.
--
-- http://api.zeromq.org/master:zmq-socket-monitor
--
-- FIXME not implemented
zmq_socket_monitor :: Zmq_socket_t -> Ptr CChar -> CInt -> IO CInt
zmq_socket_monitor = undefined

-- | Receive a message from a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-recv
zmq_recv :: Zmq_socket_t -> Ptr Word8 -> Int -> IO (Either Zmq_error Int)
zmq_recv (Zmq_socket_t socket) bytes len =
  Libzmq.Bindings.zmq_recv socket bytes (fromIntegral @Int @CSize len) 0 >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Receive a message from a ØMQ socket (non-blocking)
--
-- http://api.zeromq.org/master:zmq-recv
zmq_recv_dontwait :: Zmq_socket_t -> Ptr Word8 -> Int -> IO (Either Zmq_error Int)
zmq_recv_dontwait (Zmq_socket_t socket) bytes len =
  Libzmq.Bindings.zmq_recv__unsafe socket bytes (fromIntegral @Int @CSize len) Libzmq.Bindings._ZMQ_DONTWAIT >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))

-- | Send a message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-send
zmq_send :: Zmq_socket_t -> ByteString -> Bool -> IO (Either Zmq_error Int)
zmq_send socket message more =
  sendwith Libzmq.Bindings.zmq_send socket message (if more then Libzmq.Bindings._ZMQ_SNDMORE else 0)

-- | Send a message on a ØMQ socket (non-blocking).
--
-- http://api.zeromq.org/master:zmq-send
zmq_send_dontwait :: Zmq_socket_t -> ByteString -> Bool -> IO (Either Zmq_error Int)
zmq_send_dontwait socket message more =
  sendwith Libzmq.Bindings.zmq_send__unsafe socket message flags
  where
    flags =
      if more
        then Libzmq.Bindings._ZMQ_DONTWAIT .|. Libzmq.Bindings._ZMQ_SNDMORE
        else Libzmq.Bindings._ZMQ_DONTWAIT

-- | Send a constant-memory message on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-send-const
zmq_send_const :: Zmq_socket_t -> ByteString -> Bool -> IO (Either Zmq_error Int)
zmq_send_const socket message more =
  sendwith Libzmq.Bindings.zmq_send_const socket message (if more then Libzmq.Bindings._ZMQ_SNDMORE else 0)

-- | Send a constant-memory message on a ØMQ socket (non-blocking).
--
-- http://api.zeromq.org/master:zmq-send-const
zmq_send_const_dontwait :: Zmq_socket_t -> ByteString -> Bool -> IO (Either Zmq_error Int)
zmq_send_const_dontwait socket message more =
  sendwith Libzmq.Bindings.zmq_send_const__unsafe socket message flags
  where
    flags =
      if more
        then Libzmq.Bindings._ZMQ_DONTWAIT .|. Libzmq.Bindings._ZMQ_SNDMORE
        else Libzmq.Bindings._ZMQ_DONTWAIT

sendwith ::
  (forall socket. Ptr socket -> Ptr CChar -> CSize -> CInt -> IO CInt) ->
  Zmq_socket_t ->
  ByteString ->
  CInt ->
  IO (Either Zmq_error Int)
sendwith send0 (Zmq_socket_t socket) message flags =
  send >>= \case
    -1 -> Left <$> zmq_errno
    n -> pure (Right (fromIntegral @CInt @Int n))
  where
    send =
      ByteString.Unsafe.unsafeUseAsCStringLen message \(c_message, len) ->
        send0
          socket
          c_message
          (fromIntegral @Int @CSize len)
          flags

-- | Set a ØMQ socket option.
--
-- http://api.zeromq.org/master:zmq-setsockopt
--
-- FIXME not implemented
zmq_setsockopt :: Zmq_socket_t -> CInt -> Ptr value -> CSize -> IO CInt
zmq_setsockopt = undefined

-- | Create a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-socket
zmq_socket :: Zmq_ctx_t -> Zmq_socket_type -> IO (Either Zmq_error Zmq_socket_t)
zmq_socket (Zmq_ctx_t context) (Zmq_socket_type typ) =
  Libzmq.Bindings.zmq_socket context typ >>= \socket ->
    if socket == nullPtr then Left <$> zmq_errno else pure (Right (Zmq_socket_t socket))

-- | Stop accepting connections on a ØMQ socket.
--
-- http://api.zeromq.org/master:zmq-unbind
zmq_unbind :: Zmq_socket_t -> Text -> IO (Either Zmq_error ())
zmq_unbind (Zmq_socket_t socket) endpoint =
  Text.withCString endpoint (Libzmq.Bindings.zmq_unbind socket) >>= \case
    0 -> pure (Right ())
    _ -> Left <$> zmq_errno
