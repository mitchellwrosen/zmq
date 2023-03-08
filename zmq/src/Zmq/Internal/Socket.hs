{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Zmq.Internal.Socket
  ( Socket (..),
    CanReceive (..),
    CanSend (..),
    ThreadSafeSocket (..),
    ThreadUnsafeSocket (..),
    openThreadUnsafeSocket,
    openThreadSafeSocket,
    bind,
    unbind,
    connect,
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
import Control.Monad (join, when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.IORef
import Data.Kind (Type)
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
import GHC.IORef (IORef (..))
import GHC.MVar (MVar (..))
import GHC.STRef (STRef (..))
import Libzmq
import Libzmq.Bindings qualified
import Numeric (showHex)
import System.IO qualified as IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (Fd (..))
import Zmq.Error (Error, enrichError, throwOkError, unexpectedError)
import Zmq.Internal.Context (Context (..), globalContextRef)
import Zmq.Internal.SocketFinalizer (makeSocketFinalizer)

class Socket (socket :: Type) where
  getSocket :: socket -> (Zmq_socket -> IO a) -> IO a
  getSocket = withSocket

  withSocket :: socket -> (Zmq_socket -> IO a) -> IO a
  withSocket = undefined -- hide "minimal complete definition" haddock

  socketName :: socket -> Text
  socketName = undefined -- hide "minimal complete definition" haddock

class Socket socket => CanReceive socket where
  receive_ :: socket -> IO (Either Error ByteString)
  receive_ = undefined -- hide "minimal complete definition" haddock

class Socket socket => CanSend socket where
  send_ :: socket -> ByteString -> IO (Either Error ())
  send_ = undefined -- hide "minimal complete definition" haddock

data ThreadSafeSocket = ThreadSafeSocket
  { socketVar :: !(MVar Zmq_socket),
    name :: !Text
  }
  deriving stock (Eq)

instance Socket ThreadSafeSocket where
  getSocket (ThreadSafeSocket socketVar _) action = do
    socket <- readMVar socketVar
    action socket

  withSocket (ThreadSafeSocket socketVar _) =
    withMVar socketVar

  socketName (ThreadSafeSocket _ name) =
    name

data ThreadUnsafeSocket = ThreadUnsafeSocket
  { socket :: !Zmq_socket,
    name :: !Text,
    canary :: !(IORef ())
  }

instance Eq ThreadUnsafeSocket where
  ThreadUnsafeSocket s0 _ _ == ThreadUnsafeSocket s1 _ _ =
    s0 == s1

instance Ord ThreadUnsafeSocket where
  compare (ThreadUnsafeSocket s0 _ _) (ThreadUnsafeSocket s1 _ _) =
    compare s0 s1

instance Socket ThreadUnsafeSocket where
  withSocket (ThreadUnsafeSocket socket _name (IORef canary#)) action =
    IO \s -> keepAlive# canary# s (unIO (action socket))

  -- FIXME
  socketName ThreadUnsafeSocket {name} =
    name

-- Throws ok errors
openThreadUnsafeSocket :: Zmq_socket_type -> Text -> IO ThreadUnsafeSocket
openThreadUnsafeSocket socketType name =
  openSocket
    ( \socket -> do
        canary@(IORef (STRef canary#)) <- newIORef ()
        pure (ThingAndCanary (ThreadUnsafeSocket socket name canary) canary#)
    )
    socketType

-- Throws ok errors
openThreadSafeSocket :: Zmq_socket_type -> IO (MVar Zmq_socket)
openThreadSafeSocket =
  openSocket \socket -> do
    socketVar@(MVar canary#) <- newMVar socket
    pure (ThingAndCanary socketVar canary#)

data ThingAndCanary a
  = forall (canary# :: TYPE UnliftedRep).
    ThingAndCanary !a canary#

-- Throws ok errors
openSocket :: (Zmq_socket -> IO (ThingAndCanary a)) -> Zmq_socket_type -> IO a
openSocket wrap socketType = do
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
        finalizer <- makeSocketFinalizer (zmq_setsockopt socket) (zmq_close socket) canary
        atomicModifyIORef' socketsRef \finalizers -> (finalizer : finalizers, ())
        pure thing

-- Throws ok errors
getIntOption :: Zmq_socket -> CInt -> IO Int
getIntOption socket option = do
  let loop = do
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
  loop

-- | Bind a __socket__ to an __endpoint__.
bind :: Socket socket => socket -> Text -> IO (Either Error ())
bind socket0 endpoint =
  withSocket socket0 \socket ->
    zmq_bind socket endpoint >>= \case
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
unbind socket0 endpoint =
  withSocket socket0 \socket ->
    zmq_unbind socket endpoint >>= \case
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
connect socket0 endpoint =
  withSocket socket0 \socket ->
    zmq_connect socket endpoint >>= \case
      Left errno ->
        let err = enrichError "zmq_connect" errno
         in case errno of
              EINVAL -> throwIO err
              EMTHREAD -> pure (Left err)
              ENOCOMPATPROTO -> throwIO err
              ENOTSOCK -> throwIO err
              EPROTONOSUPPORT -> throwIO err
              ETERM -> pure (Left err)
              _ -> unexpectedError err
      Right () -> pure (Right ())

-- | Disconnect a __socket__ from an __endpoint__.
disconnect :: Socket socket => socket -> Text -> IO ()
disconnect socket0 endpoint =
  withSocket socket0 \socket ->
    zmq_disconnect socket endpoint >>= \case
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
sendOne_ :: Zmq_socket -> ByteString -> Bool -> IO ()
sendOne_ socket frame more = do
  let loop =
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
  loop

-- Send a single frame with ZMQ_DONTWAIT; on EAGAIN, returns False
-- Throws ok errors
sendOneDontWait :: Zmq_socket -> Text -> ByteString -> Bool -> IO Bool
sendOneDontWait socket name frame more = do
  when debug (debugPrintFrames socket name Outgoing (frame :| []))
  sendOneDontWait_ socket frame more

-- Throws ok errors
sendOneDontWait_ :: Zmq_socket -> ByteString -> Bool -> IO Bool
sendOneDontWait_ socket frame more = do
  let loop =
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
  loop

-- Like sendOneDontWait, but for when we know EAGAIN is impossble (so we dont set ZMQ_DONTWAIT)
-- Throws ok errors
sendOneWontBlock :: Zmq_socket -> Text -> ByteString -> Bool -> IO ()
sendOneWontBlock socket name frame more = do
  when debug (debugPrintFrames socket name Outgoing (frame :| []))
  sendOneWontBlock_ socket frame more

-- Throws ok errors
sendOneWontBlock_ :: Zmq_socket -> ByteString -> Bool -> IO ()
sendOneWontBlock_ socket frame more = do
  let loop =
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
  loop

-- Throws ok errors
sendMany :: Zmq_socket -> Text -> List.NonEmpty ByteString -> IO ()
sendMany socket name frames = do
  when debug (debugPrintFrames socket name Outgoing frames)
  sendMany_ socket frames

-- Throws ok errors
sendMany_ :: Zmq_socket -> List.NonEmpty ByteString -> IO ()
sendMany_ socket = \case
  frame :| [] -> sendOne_ socket frame False
  frame :| frames ->
    mask_ do
      sendOne_ socket frame True
      sendManyWontBlock__ socket frames

-- Throws ok errors
sendManyDontWait :: Zmq_socket -> Text -> List.NonEmpty ByteString -> IO Bool
sendManyDontWait socket name frames = do
  when debug (debugPrintFrames socket name Outgoing frames)
  sendManyDontWait_ socket frames

-- Throws ok errors
sendManyDontWait_ :: Zmq_socket -> List.NonEmpty ByteString -> IO Bool
sendManyDontWait_ socket = \case
  frame :| [] -> sendOneDontWait_ socket frame False
  frame :| frames ->
    mask_ do
      sendOneDontWait_ socket frame True >>= \case
        False -> pure False
        True -> do
          sendManyWontBlock__ socket frames
          pure True

-- Throws ok errors
sendManyWontBlock :: Zmq_socket -> Text -> List.NonEmpty ByteString -> IO ()
sendManyWontBlock socket name frames = do
  when debug (debugPrintFrames socket name Outgoing frames)
  sendManyWontBlock_ socket frames

sendManyWontBlock_ :: Zmq_socket -> List.NonEmpty ByteString -> IO ()
sendManyWontBlock_ socket = \case
  frame :| [] -> sendOneWontBlock_ socket frame False
  frame :| frames ->
    mask_ do
      sendOneWontBlock_ socket frame False
      sendManyWontBlock__ socket frames

-- Throws ok errors
sendManyWontBlock__ :: Zmq_socket -> [ByteString] -> IO ()
sendManyWontBlock__ socket =
  let loop = \case
        frame : [] -> sendOneWontBlock_ socket frame False
        frame : frames -> do
          sendOneWontBlock_ socket frame True
          loop frames
        [] -> undefined -- impossible
   in loop

-- Throws ok errors
receiveOne :: Socket socket => socket -> IO ByteString
receiveOne socket0 =
  loop
  where
    loop =
      join do
        withSocket socket0 \socket ->
          receiveOneDontWait socket (socketName socket0) <&> \case
            Nothing -> do
              blockUntilCanReceive socket
              loop
            Just frame -> pure frame

-- Receive one frame, or Nothing on EAGAIN
-- Throws ok errors
receiveOneDontWait :: Zmq_socket -> Text -> IO (Maybe ByteString)
receiveOneDontWait socket name =
  if not debug
    then do
      mask_ do
        receiveFrame socket >>= \case
          Again () -> pure Nothing
          NoMore frame -> pure (Just frame)
          More frame -> do
            receiveRest_ socket
            pure (Just frame)
    else do
      -- When debugging, we want to print all received frames, even though we only return the first
      receiveManyDontWait socket name <&> \case
        Nothing -> Nothing
        Just (frame :| _) -> Just frame

-- Receive all frames of a message
-- Throws ok errors
receiveMany :: Socket socket => socket -> IO (List.NonEmpty ByteString)
receiveMany socket0 = do
  loop
  where
    loop =
      join do
        withSocket socket0 \socket ->
          receiveManyDontWait socket (socketName socket0) <&> \case
            Nothing -> do
              blockUntilCanReceive socket
              loop
            Just frames -> pure frames

-- Receive all frames of a message, or Nothing on EAGAIN
-- Throws ok errors
receiveManyDontWait :: Zmq_socket -> Text -> IO (Maybe (List.NonEmpty ByteString))
receiveManyDontWait socket name = do
  maybeFrames <-
    mask_ do
      receiveFrame socket >>= \case
        Again () -> pure Nothing
        More frame -> do
          frames <- receiveRest socket
          pure (Just (frame :| frames))
        NoMore frame -> pure (Just (frame :| []))
  when debug (for_ maybeFrames (debugPrintFrames socket name Incoming))
  pure maybeFrames

-- Receive the rest of a multiframe message
-- Throws ok errors
receiveRest :: Zmq_socket -> IO [ByteString]
receiveRest socket =
  receiveFrameWontBlock socket >>= \case
    More frame -> do
      frames <- receiveRest socket
      pure (frame : frames)
    NoMore frame -> pure [frame]
    Again v -> absurd v

-- Throws ok errors
receiveRest_ :: Zmq_socket -> IO ()
receiveRest_ socket =
  receiveFrameWontBlock socket >>= \case
    More _ -> receiveRest_ socket
    NoMore _ -> pure ()
    Again v -> absurd v

data ReceiveF a
  = Again a
  | More ByteString
  | NoMore ByteString

-- Throws ok errors
receiveFrame :: Zmq_socket -> IO (ReceiveF ())
receiveFrame socket =
  bracket zmq_msg_init zmq_msg_close \frame -> do
    let loop = do
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
    loop

-- Throws ok errors
receiveFrameWontBlock :: Zmq_socket -> IO (ReceiveF Void)
receiveFrameWontBlock socket =
  bracket zmq_msg_init zmq_msg_close \frame -> do
    let loop = do
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
    loop

-- Throws ok errors
blockUntilCanSend :: Zmq_socket -> IO ()
blockUntilCanSend socket =
  blockUntilEvent socket Libzmq.Bindings._ZMQ_POLLOUT

-- Throws ok errors
blockUntilCanReceive :: Zmq_socket -> IO ()
blockUntilCanReceive socket =
  blockUntilEvent socket Libzmq.Bindings._ZMQ_POLLIN

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

lock :: MVar ()
lock =
  unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}

data Direction
  = Outgoing
  | Incoming

debugPrintFrames :: Zmq_socket -> Text -> Direction -> List.NonEmpty ByteString -> IO ()
debugPrintFrames (Zmq_socket socket) name direction frames = do
  let !message =
        Text.encodeUtf8 $
          Text.Lazy.toStrict $
            Text.Builder.toLazyText $
              "== "
                <> ( if Text.null name
                       then "Socket " <> Text.Builder.fromString (show socket)
                       else Text.Builder.fromText name
                   )
                <> " ==\n"
                <> foldMap formatFrame frames
                <> "\n\n"
  withMVar lock \_ -> ByteString.hPut IO.stderr message
  where
    formatFrame :: ByteString -> Text.Builder
    formatFrame frame =
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
