{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Zmq.Internal.Socket
  ( Socket (..),
    CanReceive,
    ThreadSafeSocket (..),
    ThreadUnsafeSocket (..),
    openThreadUnsafeSocket,
    openThreadSafeSocket,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    sendDontWait,
    sendWontBlock,
    sendMany,
    sendManyDontWait,
    sendManyWontBlock,
    receive,
    receiveMany,
    blockUntilCanSend,
    Sockets,
    the,
    also,
    poll,
  )
where

import Control.Concurrent (threadWaitRead)
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import Data.IORef
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Text (Text)
import Foreign.C.Types (CInt, CShort)
import GHC.Exts (TYPE, UnliftedRep, keepAlive#)
import GHC.IO (IO (..), unIO)
import GHC.IORef (IORef (..))
import GHC.MVar (MVar (..))
import GHC.STRef (STRef (..))
import Libzmq
import Libzmq.Bindings qualified
import System.Posix.Types (Fd (..))
import Zmq.Error (Error, enrichError, throwOkError, unexpectedError)
import Zmq.Internal.Context (Context (..), globalContextRef)
import Zmq.Internal.SocketFinalizer (makeSocketFinalizer)

class Socket socket where
  getSocket :: socket -> (Zmq_socket -> IO a) -> IO a
  getSocket = withSocket

  withSocket :: socket -> (Zmq_socket -> IO a) -> IO a
  withSocket = undefined -- hide "minimal complete definition" haddock

class Socket socket => CanReceive socket

newtype ThreadSafeSocket
  = ThreadSafeSocket (MVar Zmq_socket)

instance Socket ThreadSafeSocket where
  getSocket (ThreadSafeSocket socketVar) action = do
    socket <- readMVar socketVar
    action socket

  withSocket (ThreadSafeSocket socketVar) =
    withMVar socketVar

data ThreadUnsafeSocket = ThreadUnsafeSocket
  { socket :: !Zmq_socket,
    canary :: !(IORef ())
  }

instance Eq ThreadUnsafeSocket where
  ThreadUnsafeSocket s0 _ == ThreadUnsafeSocket s1 _ = s0 == s1

instance Ord ThreadUnsafeSocket where
  compare (ThreadUnsafeSocket s0 _) (ThreadUnsafeSocket s1 _) = compare s0 s1

instance Show ThreadUnsafeSocket where
  show (ThreadUnsafeSocket s0 _) = show s0

instance Socket ThreadUnsafeSocket where
  withSocket (ThreadUnsafeSocket socket (IORef canary#)) action =
    IO \s -> keepAlive# canary# s (unIO (action socket))

-- Throws ok errors
openThreadUnsafeSocket :: Zmq_socket_type -> IO ThreadUnsafeSocket
openThreadUnsafeSocket =
  openSocket \socket -> do
    canary@(IORef (STRef canary#)) <- newIORef ()
    pure (ThingAndCanary (ThreadUnsafeSocket socket canary) canary#)

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
  withSocket socket0 \socket -> bind_ socket endpoint

bind_ :: Zmq_socket -> Text -> IO (Either Error ())
bind_ socket endpoint =
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
  withSocket socket0 \socket -> unbind_ socket endpoint

unbind_ :: Zmq_socket -> Text -> IO ()
unbind_ socket endpoint =
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
  withSocket socket0 \socket -> connect_ socket endpoint

connect_ :: Zmq_socket -> Text -> IO (Either Error ())
connect_ socket endpoint =
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
  withSocket socket0 \socket -> disconnect_ socket endpoint

disconnect_ :: Zmq_socket -> Text -> IO ()
disconnect_ socket endpoint =
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

-- Send one frame
-- Throws ok errors
send :: Zmq_socket -> ByteString -> IO ()
send socket frame =
  sendOne socket frame False

-- Send one frame, returns whether it was sent
-- Throws ok errors
sendDontWait :: Zmq_socket -> ByteString -> IO Bool
sendDontWait socket frame =
  sendOneDontWait socket frame False

-- Like sendDontWait, but for when we know EAGAIN is impossble (so we dont set ZMQ_DONTWAIT)
-- Throws ok errors
sendWontBlock :: Zmq_socket -> ByteString -> IO ()
sendWontBlock socket frame =
  sendOneWontBlock socket frame False

-- Throws ok errors
sendMany :: Zmq_socket -> List.NonEmpty ByteString -> IO ()
sendMany socket = \case
  frame :| [] -> send socket frame
  frame :| frames ->
    mask_ do
      sendOne socket frame True
      sendManyWontBlock_ socket frames

-- Throws ok errors
sendManyDontWait :: Zmq_socket -> List.NonEmpty ByteString -> IO Bool
sendManyDontWait socket = \case
  frame :| [] -> sendDontWait socket frame
  frame :| frames ->
    mask_ do
      sendOneDontWait socket frame True >>= \case
        False -> pure False
        True -> do
          sendManyWontBlock_ socket frames
          pure True

-- Throws ok errors
sendManyWontBlock :: Zmq_socket -> List.NonEmpty ByteString -> IO ()
sendManyWontBlock socket = \case
  frame :| [] -> sendWontBlock socket frame
  frame :| frames ->
    mask_ do
      sendWontBlock socket frame
      sendManyWontBlock_ socket frames

-- Throws ok errors
sendManyWontBlock_ :: Zmq_socket -> [ByteString] -> IO ()
sendManyWontBlock_ socket =
  let loop = \case
        [frame] -> sendWontBlock socket frame
        frame : frames -> do
          sendOneWontBlock socket frame True
          loop frames
        [] -> undefined -- impossible
   in loop

-- Send a single frame
-- Throws ok errors
sendOne :: Zmq_socket -> ByteString -> Bool -> IO ()
sendOne socket frame more = do
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
sendOneDontWait :: Zmq_socket -> ByteString -> Bool -> IO Bool
sendOneDontWait socket frame more = do
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
sendOneWontBlock :: Zmq_socket -> ByteString -> Bool -> IO ()
sendOneWontBlock socket frame more = do
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
receive :: Zmq_socket -> IO ByteString
receive socket =
  receivef socket >>= \case
    More frame -> do
      receive_ socket
      pure frame
    NoMore frame -> pure frame

-- Throws ok errors
receive_ :: Zmq_socket -> IO ()
receive_ socket =
  receivef socket >>= \case
    More _ -> receive_ socket
    NoMore _ -> pure ()

-- Throws ok errors
receiveMany :: Zmq_socket -> IO (List.NonEmpty ByteString)
receiveMany socket =
  receivef socket >>= \case
    More frame -> do
      frames <- receives_ socket
      pure (frame :| frames)
    NoMore frame -> pure (frame :| [])

-- Throws ok errors
receives_ :: Zmq_socket -> IO [ByteString]
receives_ socket =
  receivef socket >>= \case
    More frame -> do
      frames <- receives_ socket
      pure (frame : frames)
    NoMore frame -> pure [frame]

data ReceiveF
  = More ByteString
  | NoMore ByteString

-- Throws ok errors
receivef :: Zmq_socket -> IO ReceiveF
receivef socket =
  bracket zmq_msg_init zmq_msg_close \frame -> do
    let loop = do
          zmq_msg_recv_dontwait frame socket >>= \case
            Left errno ->
              let err = enrichError "zmq_msg_recv" errno
               in case errno of
                    EAGAIN -> do
                      blockUntilEvent socket Libzmq.Bindings._ZMQ_POLLIN
                      loop
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
blockUntilEvent :: Zmq_socket -> CShort -> IO ()
blockUntilEvent socket event = do
  fd <- getIntOption socket Libzmq.Bindings._ZMQ_FD
  let loop = do
        threadWaitRead (fromIntegral @Int @Fd fd)
        events <- getIntOption socket Libzmq.Bindings._ZMQ_EVENTS
        when (events .&. fromIntegral @CShort @Int event == 0) loop
  loop

newtype Sockets
  = Sockets [SomeSocket]

data SomeSocket
  = forall socket. Socket socket => SomeSocket socket

the :: CanReceive socket => socket -> Sockets
the socket =
  also socket (Sockets [])

also :: CanReceive socket => socket -> Sockets -> Sockets
also socket =
  coerce (SomeSocket socket :)

withPollitems :: [SomeSocket] -> ([Zmq_pollitem] -> IO a) -> IO a
withPollitems events0 action =
  let go acc = \case
        [] -> action acc
        SomeSocket socket0 : zevents ->
          getSocket socket0 \socket ->
            go (Zmq_pollitem_socket socket ZMQ_POLLIN : acc) zevents
   in go [] events0

poll :: Sockets -> IO (Either Error (Int -> Bool))
poll (Sockets sockets) =
  withPollitems sockets \items0 ->
    zmq_pollitems items0 \items -> do
      let loop =
            zmq_poll items (-1) >>= \case
              Left errno ->
                let err = enrichError "zmq_poll" errno
                 in case errno of
                      EINTR -> pure (Left err)
                      EFAULT -> throwIO err
                      ETERM -> pure (Left err)
                      _ -> unexpectedError err
              Right zevents ->
                let indices = f IntSet.empty (zip [0 ..] zevents)
                 in pure (Right (`IntSet.member` indices))
      loop
  where
    f :: IntSet -> [(Int, Zmq_events)] -> IntSet
    f !acc = \case
      (i, zevents) : xs ->
        let !acc1 = if zevents /= Zmq_events 0 then IntSet.insert i acc else acc
         in f acc1 xs
      [] -> acc
