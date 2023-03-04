{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Zmq.Internal.Socket
  ( -- * Socket
    ThreadUnsafeSocket (..),
    withThreadUnsafeSocket,
    openThreadUnsafeSocket,
    openThreadSafeSocket,
    setOption,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    send1,
    receive,
  )
where

import Control.Concurrent (threadWaitRead)
import Control.Concurrent.MVar
import Control.Exception
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.IORef
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Foreign.C.Types (CInt, CShort)
import GHC.Exts (TYPE, UnliftedRep, keepAlive#)
import GHC.IO (IO (..), unIO)
import GHC.IORef (IORef (..))
import GHC.MVar (MVar (..))
import GHC.STRef (STRef (..))
import Libzmq
import Libzmq.Bindings qualified
import System.Posix.Types (Fd (..))
import Zmq.Endpoint
import Zmq.Error (Error, enrichError, enrichFunction, unexpectedError)
import Zmq.Internal (renderEndpoint)
import Zmq.Internal.Context (Context (..), globalContextRef)
import Zmq.Internal.SocketFinalizer (makeSocketFinalizer)

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

withThreadUnsafeSocket :: ThreadUnsafeSocket -> (Zmq_socket -> IO a) -> IO a
-- withThreadUnsafeSocket (ThreadUnsafeSocket socket _) action = action socket
withThreadUnsafeSocket (ThreadUnsafeSocket socket (IORef canary#)) action =
  IO \s -> keepAlive# canary# s (unIO (action socket))

openThreadUnsafeSocket :: Zmq_socket_type -> IO (Either Error ThreadUnsafeSocket)
openThreadUnsafeSocket =
  openSocket \socket -> do
    canary@(IORef (STRef canary#)) <- newIORef ()
    pure (ThingAndCanary (ThreadUnsafeSocket socket canary) canary#)

openThreadSafeSocket :: Zmq_socket_type -> IO (Either Error (MVar Zmq_socket))
openThreadSafeSocket =
  openSocket \socket -> do
    socketVar@(MVar canary#) <- newMVar socket
    pure (ThingAndCanary socketVar canary#)

data ThingAndCanary a
  = forall (canary# :: TYPE UnliftedRep).
    ThingAndCanary !a canary#

openSocket :: (Zmq_socket -> IO (ThingAndCanary a)) -> Zmq_socket_type -> IO (Either Error a)
openSocket wrap socketType = do
  Context context socketsRef <- readIORef globalContextRef
  mask_ do
    zmq_socket context socketType >>= \case
      Left errno ->
        let err = enrichError "zmq_socket" errno
         in case errno of
              EFAULT -> throwIO err
              EINVAL -> throwIO err
              EMFILE -> pure (Left err)
              ETERM -> pure (Left err)
              _ -> unexpectedError err
      Right socket -> do
        ThingAndCanary thing canary <- wrap socket
        finalizer <- makeSocketFinalizer (zmq_setsockopt socket) (zmq_close socket) canary
        atomicModifyIORef' socketsRef \finalizers -> (finalizer : finalizers, ())
        pure (Right thing)

setOption :: Zmq_socket -> Zmq_socket_option a -> a -> IO (Either Error ())
setOption socket option value = do
  let loop =
        zmq_setsockopt socket option value >>= \case
          Left errno ->
            let err = enrichError "zmq_setsockopt" errno
             in case errno of
                  EINTR -> loop
                  EINVAL -> throwIO err
                  ENOTSOCK -> throwIO err
                  ETERM -> pure (Left err)
                  _ -> unexpectedError err
          Right val -> pure (Right val)
  loop

getIntOption :: Zmq_socket -> CInt -> IO (Either Error Int)
getIntOption socket option = do
  let loop = do
        zmq_getsockopt_int socket option >>= \case
          Left errno ->
            let err = enrichError "zmq_getsockopt" errno
             in case errno of
                  EINTR -> loop
                  EINVAL -> throwIO err
                  ENOTSOCK -> throwIO err
                  ETERM -> pure (Left err)
                  _ -> unexpectedError err
          Right val -> pure (Right val)
  loop

bind :: Zmq_socket -> Endpoint transport -> IO (Either Error ())
bind socket endpoint =
  enrichFunction "zmq_bind" (zmq_bind socket (renderEndpoint endpoint))

unbind :: Zmq_socket -> Endpoint transport -> IO (Either Error ())
unbind socket endpoint =
  zmq_unbind socket (renderEndpoint endpoint) >>= \case
    Left errno ->
      let err = enrichError "zmq_unbind" errno
       in case errno of
            EINVAL -> throwIO err
            ENOENT -> pure (Right ()) -- silence these
            ENOTSOCK -> throwIO err
            ETERM -> pure (Left err)
            _ -> unexpectedError err
    Right () -> pure (Right ())

connect :: Zmq_socket -> Endpoint transport -> IO (Either Error ())
connect socket endpoint =
  enrichFunction "zmq_connect" (zmq_connect socket (renderEndpoint endpoint))

disconnect :: Zmq_socket -> Endpoint transport -> IO (Either Error ())
disconnect socket endpoint =
  enrichFunction "zmq_disconnect" (zmq_disconnect socket (renderEndpoint endpoint))

send :: Zmq_socket -> List.NonEmpty ByteString -> IO (Either Error ())
send socket message =
  let loop = \case
        [frame] -> sendf socket frame False
        frame : frames ->
          sendf socket frame True >>= \case
            Left err -> pure (Left err)
            Right () -> loop frames
        [] -> undefined -- impossible
   in loop (List.NonEmpty.toList message)

send1 :: Zmq_socket -> ByteString -> IO (Either Error ())
send1 socket frame =
  sendf socket frame False

sendf :: Zmq_socket -> ByteString -> Bool -> IO (Either Error ())
sendf socket frame more = do
  let loop = do
        zmq_send_dontwait socket frame more >>= \case
          Left errno ->
            let err = enrichError "zmq_send" errno
             in case errno of
                  EAGAIN ->
                    blockUntilEvent socket Libzmq.Bindings._ZMQ_POLLOUT >>= \case
                      Left err1 -> pure (Left err1)
                      Right () -> loop
                  EFSM -> throwIO err
                  EHOSTUNREACH -> pure (Left err)
                  EINVAL -> throwIO err
                  EINTR -> loop
                  ENOTSUP -> throwIO err
                  ENOTSOCK -> throwIO err
                  ETERM -> pure (Left err)
                  _ -> unexpectedError err
          Right _len -> pure (Right ())
  loop

receive :: Zmq_socket -> IO (Either Error (List.NonEmpty ByteString))
receive socket =
  receivef socket >>= \case
    Left err -> pure (Left err)
    Right (More frame) ->
      receive_ socket <&> \case
        Left err -> Left err
        Right frames -> Right (frame List.NonEmpty.:| reverse frames)
    Right (NoMore frame) -> pure (Right (frame List.NonEmpty.:| []))

receive_ :: Zmq_socket -> IO (Either Error [ByteString])
receive_ socket =
  receivef socket >>= \case
    Left err -> pure (Left err)
    Right (More frame) ->
      receive_ socket <&> \case
        Left err -> Left err
        Right frames -> Right (frame : frames)
    Right (NoMore frame) -> pure (Right [frame])

data ReceiveF
  = More ByteString
  | NoMore ByteString

receivef :: Zmq_socket -> IO (Either Error ReceiveF)
receivef socket =
  bracket zmq_msg_init zmq_msg_close \frame -> do
    let loop = do
          zmq_msg_recv_dontwait frame socket >>= \case
            Left errno ->
              let err = enrichError "zmq_msg_recv" errno
               in case errno of
                    EAGAIN ->
                      blockUntilEvent socket Libzmq.Bindings._ZMQ_POLLIN >>= \case
                        Left err1 -> pure (Left err1)
                        Right () -> loop
                    EFSM -> throwIO err
                    EINTR -> loop
                    ENOTSOCK -> throwIO err
                    ENOTSUP -> throwIO err
                    ETERM -> pure (Left err)
                    _ -> unexpectedError err
            Right _len -> do
              bytes <- zmq_msg_data frame
              zmq_msg_more frame <&> \case
                False -> Right (NoMore bytes)
                True -> Right (More bytes)
    loop

blockUntilEvent :: Zmq_socket -> CShort -> IO (Either Error ())
blockUntilEvent socket event =
  getIntOption socket Libzmq.Bindings._ZMQ_FD >>= \case
    Left err -> pure (Left err)
    Right fd -> do
      let loop = do
            threadWaitRead (fromIntegral @Int @Fd fd)
            getIntOption socket Libzmq.Bindings._ZMQ_EVENTS >>= \case
              Left err1 -> pure (Left err1)
              Right events ->
                if events .&. fromIntegral @CShort @Int event == 0
                  then loop
                  else pure (Right ())
      loop
