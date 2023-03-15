{-# LANGUAGE MagicHash #-}

module Zmq.Internal.Socket
  ( Socket (..),
    keepingSocketAlive,
    CanReceive (..),
    CanReceives (..),
    CanSend (..),
    ThingAndCanary (..),
    openWith,
    bind,
    unbind,
    connect,
    connect_,
    disconnect,
    sendOneDontWait,
    sendOneWontBlock,
    sendMany,
    sendManyDontWait,
    sendManyWontBlock,
    receiveOne,
    receiveMany,
    blockUntilCanSend,
    blockUntilCanReceive,
  )
where

import Control.Concurrent (threadWaitRead)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.IORef
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder
import Data.Void (Void, absurd)
import Data.Word (Word8)
import Foreign.C.Types (CInt, CShort)
import GHC.Exts (TYPE, UnliftedRep, keepAlive#)
import GHC.IO (IO (..), unIO)
import Libzmq
import Libzmq.Bindings qualified
import Numeric (showHex)
import System.IO qualified as IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (Fd (..))
import Zmq.Error (Error, catchingOkErrors, enrichError, throwOkError, unexpectedError)
import Zmq.Internal.Context (Context (..), globalContextRef)
import {-# SOURCE #-} Zmq.Internal.Options (Options)
import Zmq.Internal.SocketFinalizer (makeSocketFinalizer)

class Socket socket where
  openSocket :: Options socket -> IO (Either Error socket)
  openSocket = undefined -- hide "minimal complete definition" haddock

  getSocket :: socket -> Zmq_socket
  getSocket = undefined -- hide "minimal complete definition" haddock

  withSocket :: socket -> IO a -> IO a
  withSocket = undefined -- hide "minimal complete definition" haddock

  socketName :: socket -> Text
  socketName = undefined -- hide "minimal complete definition" haddock

keepingSocketAlive :: Zmq_socket -> IO a -> IO a
keepingSocketAlive socket action =
  IO \s -> keepAlive# socket s (unIO action)

class Socket socket => CanReceive socket where
  receive_ :: socket -> IO (Either Error ByteString)
  receive_ = undefined -- hide "minimal complete definition" haddock

class Socket socket => CanReceives socket where
  receives_ :: socket -> IO (Either Error [ByteString])
  receives_ = undefined -- hide "minimal complete definition" haddock

class Socket socket => CanSend socket where
  send_ :: socket -> ByteString -> IO (Either Error ())
  send_ = undefined -- hide "minimal complete definition" haddock

data ThingAndCanary a
  = forall (canary# :: TYPE UnliftedRep).
    ThingAndCanary !a canary#

-- Throws ok errors
openWith :: (Zmq_socket -> IO (ThingAndCanary a)) -> Zmq_socket_type -> IO a
openWith wrap socketType = do
  Context context socketsRef <- readIORef globalContextRef
  mask_ do
    zmq_socket context socketType >>= \case
      Left errno ->
        let err = enrichError "zmq_socket" errno
         in case errno of
              EFAULT -> throwIO err
              EINVAL -> throwIO err
              EMFILE -> throwOkError err
              ETERM -> throwOkError err
              _ -> unexpectedError err
      Right socket -> do
        ThingAndCanary thing canary <- wrap socket
        finalizer <- makeSocketFinalizer (zmq_close socket) canary
        atomicModifyIORef' socketsRef \finalizers -> (finalizer : finalizers, ())
        pure thing

-- Throws ok errors
getIntOption :: Zmq_socket -> CInt -> IO Int
getIntOption socket option =
  zmq_getsockopt_int socket option >>= \case
    Left errno ->
      let err = enrichError "zmq_getsockopt" errno
       in case errno of
            EINTR -> throwOkError err
            EINVAL -> throwIO err
            ENOTSOCK -> throwIO err
            ETERM -> throwOkError err
            _ -> unexpectedError err
    Right val -> pure val

-- | Bind a __socket__ to an __endpoint__.
bind :: Socket socket => socket -> Text -> IO (Either Error ())
bind socket endpoint =
  withSocket socket do
    zmq_bind (getSocket socket) endpoint >>= \case
      Left errno ->
        let err = enrichError "zmq_bind" errno
         in case errno of
              EADDRINUSE -> pure (Left err)
              EADDRNOTAVAIL -> throwIO err
              EINVAL -> throwIO err
              EMTHREAD -> pure (Left err)
              ENOCOMPATPROTO -> throwIO err
              ENODEV -> throwIO err
              ENOTSOCK -> throwIO err
              EPROTONOSUPPORT -> throwIO err
              ETERM -> pure (Left err)
              _ -> unexpectedError err
      Right () -> pure (Right ())

-- | Unbind a __socket__ from an __endpoint__.
unbind :: Socket socket => socket -> Text -> IO ()
unbind socket endpoint =
  withSocket socket do
    zmq_unbind (getSocket socket) endpoint >>= \case
      Left errno ->
        let err = enrichError "zmq_unbind" errno
         in case errno of
              -- These aren't very interesting to report to the user; in all cases, we can say "ok, we
              -- disconnected", so we prefer the cleaner return type with no Either.
              EINVAL -> pure ()
              ENOENT -> pure ()
              ENOTSOCK -> pure ()
              ETERM -> pure ()
              _ -> unexpectedError err
      Right () -> pure ()

-- | Connect a __socket__ to an __endpoint__.
connect :: Socket socket => socket -> Text -> IO (Either Error ())
connect socket endpoint =
  catchingOkErrors (connect_ socket endpoint)

-- Throws ok errors
connect_ :: Socket socket => socket -> Text -> IO ()
connect_ socket endpoint =
  withSocket socket do
    zmq_connect (getSocket socket) endpoint >>= \case
      Left errno ->
        let err = enrichError "zmq_connect" errno
         in case errno of
              EINVAL -> throwIO err
              EMTHREAD -> throwOkError err
              ENOCOMPATPROTO -> throwIO err
              ENOTSOCK -> throwIO err
              EPROTONOSUPPORT -> throwIO err
              ETERM -> throwOkError err
              _ -> unexpectedError err
      Right () -> pure ()

-- | Disconnect a __socket__ from an __endpoint__.
disconnect :: Socket socket => socket -> Text -> IO ()
disconnect socket endpoint =
  withSocket socket do
    zmq_disconnect (getSocket socket) endpoint >>= \case
      Left errno ->
        let err = enrichError "zmq_disconnect" errno
         in case errno of
              -- These aren't very interesting to report to the user; in all cases, we can say "ok, we
              -- disconnected", so we prefer the cleaner return type with no Either.
              EINVAL -> pure ()
              ENOENT -> pure ()
              ENOTSOCK -> pure ()
              ETERM -> pure ()
              _ -> unexpectedError err
      Right () -> pure ()

-- Throws ok errors
zsendOne :: Zmq_socket -> ByteString -> Bool -> IO ()
zsendOne socket frame more =
  zmq_send socket frame (if more then ZMQ_SNDMORE else mempty) >>= \case
    Left errno ->
      let err = enrichError "zmq_send" errno
       in case errno of
            EFSM -> throwIO err
            EHOSTUNREACH -> throwOkError err
            EINVAL -> throwIO err
            EINTR -> throwOkError err
            ENOTSUP -> throwIO err
            ENOTSOCK -> throwIO err
            ETERM -> throwOkError err
            _ -> unexpectedError err
    Right _len -> pure ()

-- Send a single frame with ZMQ_DONTWAIT; on EAGAIN, returns False
-- Throws ok errors
sendOneDontWait :: Socket socket => socket -> ByteString -> Bool -> IO Bool
sendOneDontWait socket frame more = do
  when debug (debugPrintFrames socket Outgoing (frame :| []))
  withSocket socket do
    zsendOneDontWait (getSocket socket) frame more

-- Throws ok errors
zsendOneDontWait :: Zmq_socket -> ByteString -> Bool -> IO Bool
zsendOneDontWait socket frame more =
  zmq_send__unsafe socket frame (if more then ZMQ_DONTWAIT <> ZMQ_SNDMORE else ZMQ_DONTWAIT) >>= \case
    Left errno ->
      let err = enrichError "zmq_send" errno
       in case errno of
            EAGAIN -> pure False
            EFSM -> throwIO err
            EHOSTUNREACH -> throwOkError err
            EINVAL -> throwIO err
            EINTR -> throwOkError err
            ENOTSUP -> throwIO err
            ENOTSOCK -> throwIO err
            ETERM -> throwOkError err
            _ -> unexpectedError err
    Right _len -> pure True

-- Like sendOneDontWait, but for when we know EAGAIN is impossble (so we dont set ZMQ_DONTWAIT)
-- Throws ok errors
sendOneWontBlock :: Socket socket => socket -> ByteString -> Bool -> IO ()
sendOneWontBlock socket frame more = do
  when debug (debugPrintFrames socket Outgoing (frame :| []))
  withSocket socket do
    zsendOneWontBlock (getSocket socket) frame more

-- Throws ok errors
zsendOneWontBlock :: Zmq_socket -> ByteString -> Bool -> IO ()
zsendOneWontBlock socket frame more =
  zmq_send__unsafe socket frame (if more then ZMQ_SNDMORE else mempty) >>= \case
    Left errno ->
      let err = enrichError "zmq_send" errno
       in case errno of
            EFSM -> throwIO err
            EHOSTUNREACH -> throwOkError err
            EINVAL -> throwIO err
            EINTR -> throwOkError err
            ENOTSUP -> throwIO err
            ENOTSOCK -> throwIO err
            ETERM -> throwOkError err
            _ -> unexpectedError err
    Right _len -> pure ()

-- Throws ok errors
sendMany :: Socket socket => socket -> List.NonEmpty ByteString -> IO ()
sendMany socket frames = do
  when debug (debugPrintFrames socket Outgoing frames)
  withSocket socket do
    zsendMany (getSocket socket) frames

-- Throws ok errors
zsendMany :: Zmq_socket -> List.NonEmpty ByteString -> IO ()
zsendMany socket = \case
  frame :| [] -> zsendOne socket frame False
  frame :| frames ->
    mask_ do
      zsendOne socket frame True
      zsendManyWontBlock0 socket frames

-- Throws ok errors
sendManyDontWait :: Socket socket => socket -> List.NonEmpty ByteString -> IO Bool
sendManyDontWait socket frames = do
  when debug (debugPrintFrames socket Outgoing frames)
  withSocket socket do
    zsendManyDontWait (getSocket socket) frames

-- Throws ok errors
zsendManyDontWait :: Zmq_socket -> List.NonEmpty ByteString -> IO Bool
zsendManyDontWait socket = \case
  frame :| [] -> zsendOneDontWait socket frame False
  frame :| frames ->
    mask_ do
      zsendOneDontWait socket frame True >>= \case
        False -> pure False
        True -> do
          zsendManyWontBlock0 socket frames
          pure True

-- Throws ok errors
sendManyWontBlock :: Socket socket => socket -> List.NonEmpty ByteString -> IO ()
sendManyWontBlock socket frames = do
  when debug (debugPrintFrames socket Outgoing frames)
  withSocket socket do
    zsendManyWontBlock1 (getSocket socket) frames

zsendManyWontBlock1 :: Zmq_socket -> List.NonEmpty ByteString -> IO ()
zsendManyWontBlock1 socket = \case
  frame :| [] -> zsendOneWontBlock socket frame False
  frame :| frames ->
    mask_ do
      zsendOneWontBlock socket frame True
      zsendManyWontBlock0 socket frames

-- Throws ok errors
zsendManyWontBlock0 :: Zmq_socket -> [ByteString] -> IO ()
zsendManyWontBlock0 socket =
  let loop = \case
        frame : [] -> zsendOneWontBlock socket frame False
        frame : frames -> do
          zsendOneWontBlock socket frame True
          loop frames
        [] -> undefined -- impossible
   in loop

-- Receive one frame
-- Throws ok errors
receiveOne :: Socket socket => socket -> IO ByteString
receiveOne socket =
  loop
  where
    loop =
      receiveOneDontWait socket >>= \case
        Nothing -> do
          blockUntilCanReceive socket
          loop
        Just frame -> pure frame

-- Receive one frame, or Nothing on EAGAIN
-- Throws ok errors
receiveOneDontWait :: Socket socket => socket -> IO (Maybe ByteString)
receiveOneDontWait socket =
  if not debug
    then do
      withSocket socket do
        mask_ do
          zreceiveOne zsocket >>= \case
            Again () -> pure Nothing
            NoMore frame -> pure (Just frame)
            More frame -> do
              receiveRest_ zsocket
              pure (Just frame)
    else do
      -- When debugging, we want to print all received frames, even though we only return the first
      receiveManyDontWait socket <&> \case
        Nothing -> Nothing
        Just (frame :| _) -> Just frame
  where
    zsocket = getSocket socket

-- Receive all frames of a message
-- Throws ok errors
receiveMany :: Socket socket => socket -> IO (List.NonEmpty ByteString)
receiveMany socket = do
  loop
  where
    loop =
      receiveManyDontWait socket >>= \case
        Nothing -> do
          blockUntilCanReceive socket
          loop
        Just frames -> pure frames

-- Receive all frames of a message, or Nothing on EAGAIN
-- Throws ok errors
receiveManyDontWait :: Socket socket => socket -> IO (Maybe (List.NonEmpty ByteString))
receiveManyDontWait socket =
  withSocket socket do
    maybeFrames <-
      mask_ do
        zreceiveOne zsocket >>= \case
          Again () -> pure Nothing
          More frame -> do
            frames <- zreceiveManyWontBlock0 zsocket
            pure (Just (frame :| frames))
          NoMore frame -> pure (Just (frame :| []))
    when debug (for_ maybeFrames (debugPrintFrames socket Incoming))
    pure maybeFrames
  where
    zsocket = getSocket socket

-- Receive all frames of a message
-- Throws ok errors
zreceiveManyWontBlock0 :: Zmq_socket -> IO [ByteString]
zreceiveManyWontBlock0 socket =
  zreceiveOneWontBlock socket >>= \case
    More frame -> do
      frames <- zreceiveManyWontBlock0 socket
      pure (frame : frames)
    NoMore frame -> pure [frame]
    Again v -> absurd v

-- Throws ok errors
receiveRest_ :: Zmq_socket -> IO ()
receiveRest_ socket =
  zreceiveOneWontBlock socket >>= \case
    More _ -> receiveRest_ socket
    NoMore _ -> pure ()
    Again v -> absurd v

data Zreceive a
  = Again a
  | More ByteString
  | NoMore ByteString

-- Throws ok errors
zreceiveOne :: Zmq_socket -> IO (Zreceive ())
zreceiveOne socket =
  bracket zmq_msg_init zmq_msg_close \frame ->
    zmq_msg_recv_dontwait frame socket >>= \case
      Left errno ->
        let err = enrichError "zmq_msg_recv" errno
         in case errno of
              EAGAIN -> pure (Again ())
              EFSM -> throwIO err
              EINTR -> throwOkError err
              ENOTSOCK -> throwIO err
              ENOTSUP -> throwIO err
              ETERM -> throwOkError err
              _ -> unexpectedError err
      Right _len -> do
        bytes <- zmq_msg_data frame
        zmq_msg_more frame <&> \case
          False -> NoMore bytes
          True -> More bytes

-- Throws ok errors
zreceiveOneWontBlock :: Zmq_socket -> IO (Zreceive Void)
zreceiveOneWontBlock socket =
  bracket zmq_msg_init zmq_msg_close \frame ->
    zmq_msg_recv_dontwait frame socket >>= \case
      Left errno ->
        let err = enrichError "zmq_msg_recv" errno
         in case errno of
              EFSM -> throwIO err
              EINTR -> throwOkError err
              ENOTSOCK -> throwIO err
              ENOTSUP -> throwIO err
              ETERM -> throwOkError err
              _ -> unexpectedError err
      Right _len -> do
        bytes <- zmq_msg_data frame
        zmq_msg_more frame <&> \case
          False -> NoMore bytes
          True -> More bytes

-- Throws ok errors
blockUntilCanSend :: Socket socket => socket -> IO ()
blockUntilCanSend socket =
  blockUntilEvent (getSocket socket) Libzmq.Bindings._ZMQ_POLLOUT

-- Throws ok errors
blockUntilCanReceive :: Socket socket => socket -> IO ()
blockUntilCanReceive socket =
  blockUntilEvent (getSocket socket) Libzmq.Bindings._ZMQ_POLLIN

-- Throws ok errors
blockUntilEvent :: Zmq_socket -> CShort -> IO ()
blockUntilEvent socket event = do
  fd <- getIntOption socket Libzmq.Bindings._ZMQ_FD
  let loop = do
        threadWaitRead (fromIntegral @Int @Fd fd)
        events <- getIntOption socket Libzmq.Bindings._ZMQ_EVENTS
        when (events .&. fromIntegral @CShort @Int event == 0) loop
  loop

------------------------------------------------------------------------------------------------------------------------
-- Debugging utils
--
-- TODO make these context/socket options
-- debug :: (Text -> IO ()) -> Options a

debug :: Bool
debug = True

debuglock :: MVar ()
debuglock =
  unsafePerformIO (newMVar ())
{-# NOINLINE debuglock #-}

data Direction
  = Outgoing
  | Incoming

debugPrintFrames :: Socket socket => socket -> Direction -> List.NonEmpty ByteString -> IO ()
debugPrintFrames socket direction frames = do
  let !message =
        Text.encodeUtf8 $
          Text.Lazy.toStrict $
            Text.Builder.toLazyText $
              "== "
                <> ( if Text.null name
                       then "Socket " <> Text.Builder.fromString (show (getSocket socket))
                       else Text.Builder.fromText name
                   )
                <> " ==\n"
                <> foldMap (formatFrame direction) frames
                <> "\n"
  withMVar debuglock \_ ->
    ByteString.hPut IO.stderr message
  where
    name = socketName socket

formatFrame :: Direction -> ByteString -> Text.Builder
formatFrame direction frame =
  (case direction of Outgoing -> "  >> "; Incoming -> "  << ")
    <> if ByteString.null frame
      then "\n"
      else
        formatBytes frame
          <> case Text.decodeUtf8' frame of
            Left _ -> mempty
            Right frame1 -> " " <> Text.Builder.fromText frame1
          <> "\n"

formatBytes :: ByteString -> Text.Builder
formatBytes bytes =
  "0x" <> List.foldl' f mempty (ByteString.unpack bytes)
  where
    f :: Text.Builder -> Word8 -> Text.Builder
    f acc w =
      if w < 15
        then acc <> "0" <> x
        else acc <> x
      where
        x = Text.Builder.fromString (showHex w "")
