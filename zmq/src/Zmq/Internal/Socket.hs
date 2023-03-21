{-# LANGUAGE MagicHash #-}

module Zmq.Internal.Socket
  ( Socket (..),
    Extra (..),
    CanSend (..),
    CanReceive (..),
    CanReceives (..),
    openSocket,
    usingSocket,
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
import Data.Coerce (coerce)
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
import Data.Word (Word8)
import Foreign (Ptr)
import Foreign.C.Types (CInt, CShort)
import GHC.Base (Symbol)
import GHC.MVar (MVar (..))
import Libzmq
import Libzmq.Bindings qualified
import Numeric (showHex)
import System.IO qualified as IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (Fd (..))
import Zmq.Error (Error, catchingOkErrors, enrichError, throwOkError, unexpectedError)
import Zmq.Internal.Context (globalContextRef, globalSocketFinalizersRef)
import Zmq.Internal.IO (keepAlive)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.SocketFinalizer (makeSocketFinalizer)

type role Socket nominal

data Socket (a :: Symbol) = Socket
  { zsocket :: !Zmq_socket,
    lock :: !(MVar ()),
    name :: !Text,
    extra :: !(Extra a)
  }

instance Eq (Socket a) where
  Socket x _ _ _ == Socket y _ _ _ = x == y
  Socket x _ _ _ /= Socket y _ _ _ = x /= y

instance Ord (Socket a) where
  compare (Socket x _ _ _) (Socket y _ _ _) = compare x y
  Socket x _ _ _ < Socket y _ _ _ = x < y
  Socket x _ _ _ <= Socket y _ _ _ = x <= y
  Socket x _ _ _ > Socket y _ _ _ = x > y
  Socket x _ _ _ >= Socket y _ _ _ = x >= y

data Extra (a :: Symbol) where
  DealerExtra :: Extra "DEALER"
  PairExtra :: Extra "PAIR"
  PubExtra :: Extra "PUB"
  PullExtra :: Extra "PULL"
  PushExtra :: Extra "PUSH"
  RepExtra :: Extra "REP"
  -- The last message we received, if any. See Note [Req message buffer] for details.
  ReqExtra :: !(IORef (Maybe (List.NonEmpty ByteString))) -> Extra "REQ"
  RouterExtra :: Extra "ROUTER"
  SubExtra :: Extra "SUB"
  XPubExtra :: Extra "XPUB"
  XSubExtra :: Extra "XSUB"

class CanSend a where
  send_ :: a -> ByteString -> IO (Either Error ())
  send_ = undefined -- hide "minimal complete definition" haddock

class CanReceive a where
  receive_ :: a -> IO (Either Error ByteString)
  receive_ = undefined -- hide "minimal complete definition" haddock

class CanReceives a where
  receives_ :: a -> IO (Either Error [ByteString])
  receives_ = undefined -- hide "minimal complete definition" haddock

-- Throws ok errors
openSocket :: Zmq_socket_type -> Options.Options (Socket a) -> Extra a -> IO (Socket a)
openSocket socketType options extra = do
  context <- readIORef globalContextRef
  lock@(MVar canary#) <- newMVar ()
  zsocket <-
    mask_ do
      zsocket <- zhs_socket context socketType
      finalizer <- makeSocketFinalizer (zmq_close zsocket) canary#
      atomicModifyIORef' globalSocketFinalizersRef \finalizers -> (finalizer : finalizers, ())
      pure zsocket
  Options.setSocketOptions zsocket options
  pure
    Socket
      { zsocket,
        lock,
        name = Options.optionsName options,
        extra
      }

usingSocket :: Socket a -> IO b -> IO b
usingSocket Socket {lock, zsocket} action =
  withMVar lock \_ ->
    keepAlive zsocket action

-- | Bind a __socket__ to an __endpoint__.
bind :: Socket a -> Text -> IO (Either Error ())
bind socket endpoint =
  catchingOkErrors do
    usingSocket socket do
      zhs_bind (zsocket socket) endpoint

-- | Unbind a __socket__ from an __endpoint__.
unbind :: Socket a -> Text -> IO ()
unbind socket endpoint =
  usingSocket socket do
    zhs_unbind (zsocket socket) endpoint

-- | Connect a __socket__ to an __endpoint__.
connect :: Socket a -> Text -> IO (Either Error ())
connect socket endpoint =
  catchingOkErrors do
    connect_ socket endpoint

-- Throws ok errors
connect_ :: Socket a -> Text -> IO ()
connect_ socket endpoint =
  usingSocket socket do
    zhs_connect (zsocket socket) endpoint

-- | Disconnect a __socket__ from an __endpoint__.
disconnect :: Socket a -> Text -> IO ()
disconnect socket endpoint =
  usingSocket socket do
    zhs_disconnect (zsocket socket) endpoint

-- Send a single frame with ZMQ_DONTWAIT; on EAGAIN, returns False
-- Throws ok errors
sendOneDontWait :: Socket a -> ByteString -> Bool -> IO Bool
sendOneDontWait socket frame more = do
  when debug (debugPrintFrames socket Outgoing (frame :| []))
  usingSocket socket do
    zhs_send_frame_dontwait (zsocket socket) frame more

-- Like sendOneDontWait, but for when we know EAGAIN is impossble (so we dont set ZMQ_DONTWAIT)
-- Throws ok errors
sendOneWontBlock :: Socket a -> ByteString -> Bool -> IO ()
sendOneWontBlock socket frame more = do
  when debug (debugPrintFrames socket Outgoing (frame :| []))
  usingSocket socket do
    zhs_send_frame_wontblock (zsocket socket) frame more

-- Throws ok errors
sendMany :: Socket a -> List.NonEmpty ByteString -> IO ()
sendMany socket frames = do
  when debug (debugPrintFrames socket Outgoing frames)
  usingSocket socket do
    zsendMany (zsocket socket) frames

-- Throws ok errors
zsendMany :: Zmq_socket -> List.NonEmpty ByteString -> IO ()
zsendMany socket = \case
  frame :| [] -> zhs_send_frame socket frame False
  frame :| frames ->
    mask_ do
      zhs_send_frame socket frame True
      zsendManyWontBlock0 socket frames

-- Throws ok errors
sendManyDontWait :: Socket a -> List.NonEmpty ByteString -> IO Bool
sendManyDontWait socket frames = do
  when debug (debugPrintFrames socket Outgoing frames)
  usingSocket socket do
    zsendManyDontWait (zsocket socket) frames

-- Throws ok errors
zsendManyDontWait :: Zmq_socket -> List.NonEmpty ByteString -> IO Bool
zsendManyDontWait socket = \case
  frame :| [] -> zhs_send_frame_dontwait socket frame False
  frame :| frames ->
    mask_ do
      zhs_send_frame_dontwait socket frame True >>= \case
        False -> pure False
        True -> do
          zsendManyWontBlock0 socket frames
          pure True

-- Throws ok errors
sendManyWontBlock :: Socket a -> List.NonEmpty ByteString -> IO ()
sendManyWontBlock socket frames = do
  when debug (debugPrintFrames socket Outgoing frames)
  usingSocket socket do
    zsendManyWontBlock1 (zsocket socket) frames

zsendManyWontBlock1 :: Zmq_socket -> List.NonEmpty ByteString -> IO ()
zsendManyWontBlock1 socket = \case
  frame :| [] -> zhs_send_frame_wontblock socket frame False
  frame :| frames ->
    mask_ do
      zhs_send_frame_wontblock socket frame True
      zsendManyWontBlock0 socket frames

-- Throws ok errors
zsendManyWontBlock0 :: Zmq_socket -> [ByteString] -> IO ()
zsendManyWontBlock0 socket =
  let loop = \case
        frame : [] -> zhs_send_frame_wontblock socket frame False
        frame : frames -> do
          zhs_send_frame_wontblock socket frame True
          loop frames
        [] -> undefined -- impossible
   in loop

-- Receive one frame
-- Throws ok errors
receiveOne :: Socket a -> IO ByteString
receiveOne socket@Socket {zsocket} =
  loop
  where
    loop =
      receiveOneDontWait socket >>= \case
        Nothing -> do
          blockUntilCanReceive zsocket
          loop
        Just frame -> pure frame

-- Receive one frame, or Nothing on EAGAIN
-- Throws ok errors
receiveOneDontWait :: Socket a -> IO (Maybe ByteString)
receiveOneDontWait socket@Socket {zsocket} =
  if not debug
    then do
      usingSocket socket do
        mask_ do
          zhs_recv_frame_dontwait zsocket >>= \case
            Again -> pure Nothing
            NoMore frame -> pure (Just frame)
            More frame -> do
              receiveRest_ zsocket
              pure (Just frame)
    else do
      -- When debugging, we want to print all received frames, even though we only return the first
      receiveManyDontWait socket <&> \case
        Nothing -> Nothing
        Just (frame :| _) -> Just frame

-- Receive all frames of a message
-- Throws ok errors
receiveMany :: Socket a -> IO (List.NonEmpty ByteString)
receiveMany socket@Socket {zsocket} =
  loop
  where
    loop =
      receiveManyDontWait socket >>= \case
        Nothing -> do
          blockUntilCanReceive zsocket
          loop
        Just frames -> pure frames

-- Receive all frames of a message, or Nothing on EAGAIN
-- Throws ok errors
receiveManyDontWait :: Socket a -> IO (Maybe (List.NonEmpty ByteString))
receiveManyDontWait socket@Socket {zsocket} =
  usingSocket socket do
    maybeFrames <-
      mask_ do
        zhs_recv_frame_dontwait zsocket >>= \case
          Again -> pure Nothing
          More frame -> do
            frames <- zreceiveManyWontBlock0 zsocket
            pure (Just (frame :| frames))
          NoMore frame -> pure (Just (frame :| []))
    when debug (for_ maybeFrames (debugPrintFrames socket Incoming))
    pure maybeFrames

-- Receive all frames of a message
-- Throws ok errors
zreceiveManyWontBlock0 :: Zmq_socket -> IO [ByteString]
zreceiveManyWontBlock0 socket =
  zhs_recv_frame_wontblock socket >>= \case
    More frame -> do
      frames <- zreceiveManyWontBlock0 socket
      pure (frame : frames)
    NoMore frame -> pure [frame]

-- Throws ok errors
receiveRest_ :: Zmq_socket -> IO ()
receiveRest_ socket =
  zhs_recv_frame_wontblock_ socket >>= \case
    False -> pure ()
    True -> receiveRest_ socket

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
  fd <- zhs_getsockopt_int socket Libzmq.Bindings._ZMQ_FD
  let loop = do
        threadWaitRead (fromIntegral @Int @Fd fd)
        events <- zhs_getsockopt_int socket Libzmq.Bindings._ZMQ_EVENTS
        when (events .&. fromIntegral @CShort @Int event == 0) loop
  loop

------------------------------------------------------------------------------------------------------------------------
-- Lower-level operations
--
-- These wrap `libzmq` operations, and translate each error to either an exception (because it's the user's fault) or an
-- "ok error" (still an exception, but caught later with `catchingOkErrors`), because it's not the user's fault, but we
-- want short-circuiting syntax internally (else we'll be deeply nesting Either pattern matches all over the place).

zhs_socket :: Zmq_ctx -> Zmq_socket_type -> IO Zmq_socket
zhs_socket context socketType = do
  zmq_socket context socketType >>= \case
    Left errno ->
      let err = enrichError "zmq_socket" errno
       in case errno of
            EFAULT -> throwIO err
            EINVAL -> throwIO err
            EMFILE -> throwOkError err
            ETERM -> throwOkError err
            _ -> unexpectedError err
    Right socket -> pure socket

zhs_getsockopt_int :: Zmq_socket -> CInt -> IO Int
zhs_getsockopt_int socket option =
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

zhs_bind :: Zmq_socket -> Text -> IO ()
zhs_bind socket endpoint =
  zmq_bind socket endpoint >>= \case
    Left errno ->
      let err = enrichError "zmq_bind" errno
       in case errno of
            EADDRINUSE -> throwOkError err
            EADDRNOTAVAIL -> throwIO err
            EINVAL -> throwIO err
            EMTHREAD -> throwOkError err
            ENOCOMPATPROTO -> throwIO err
            ENODEV -> throwIO err
            ENOTSOCK -> throwIO err
            EPROTONOSUPPORT -> throwIO err
            ETERM -> throwOkError err
            _ -> unexpectedError err
    Right () -> pure ()

zhs_unbind :: Zmq_socket -> Text -> IO ()
zhs_unbind socket endpoint =
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

zhs_connect :: Zmq_socket -> Text -> IO ()
zhs_connect socket endpoint =
  zmq_connect socket endpoint >>= \case
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

zhs_disconnect :: Zmq_socket -> Text -> IO ()
zhs_disconnect socket endpoint =
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

zhs_send_frame :: Zmq_socket -> ByteString -> Bool -> IO ()
zhs_send_frame socket frame more =
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

zhs_send_frame_dontwait :: Zmq_socket -> ByteString -> Bool -> IO Bool
zhs_send_frame_dontwait socket frame more =
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

zhs_send_frame_wontblock :: Zmq_socket -> ByteString -> Bool -> IO ()
zhs_send_frame_wontblock socket frame more =
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

data Frame :: Bool -> Type where
  Again :: Frame 'False
  More :: !ByteString -> Frame a
  NoMore :: !ByteString -> Frame a

zhs_recv_frame_dontwait :: Zmq_socket -> IO (Frame 'False)
zhs_recv_frame_dontwait socket =
  zhs_with_frame \frame ->
    zmq_msg_recv_dontwait frame socket >>= \case
      Left errno ->
        let err = enrichError "zmq_msg_recv" errno
         in case errno of
              EAGAIN -> pure Again
              EFSM -> throwIO err
              EINTR -> throwOkError err
              ENOTSOCK -> throwIO err
              ENOTSUP -> throwIO err
              ETERM -> throwOkError err
              _ -> unexpectedError err
      Right _len -> zhs_frame frame

zhs_recv_frame_wontblock :: Zmq_socket -> IO (Frame 'True)
zhs_recv_frame_wontblock socket =
  zhs_with_frame \frame ->
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
      Right _len -> zhs_frame frame

zhs_recv_frame_wontblock_ :: Zmq_socket -> IO Bool
zhs_recv_frame_wontblock_ socket =
  zhs_with_frame \frame ->
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
      Right _len -> zmq_msg_more frame

zhs_with_frame :: (Zmq_msg -> IO a) -> IO a
zhs_with_frame =
  bracket zmq_msg_init zmq_msg_close

zhs_frame :: Zmq_msg -> IO (Frame a)
zhs_frame frame = do
  bytes <- zmq_msg_data frame
  zmq_msg_more frame <&> \case
    False -> NoMore bytes
    True -> More bytes

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

debugPrintFrames :: Socket a -> Direction -> List.NonEmpty ByteString -> IO ()
debugPrintFrames socket direction frames = do
  let !message =
        Text.encodeUtf8 $
          Text.Lazy.toStrict $
            Text.Builder.toLazyText $
              "== "
                <> ( if Text.null (name socket)
                       then "Socket " <> Text.Builder.fromString (show (coerce @Zmq_socket @(Ptr ()) (zsocket socket)))
                       else Text.Builder.fromText (name socket)
                   )
                <> " ==\n"
                <> foldMap (formatFrame direction) frames
                <> "\n"
  withMVar debuglock \_ ->
    ByteString.hPut IO.stderr message

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
