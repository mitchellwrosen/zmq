{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Zmq.Internal.Context
  ( Context (..),
    globalContextRef,
    run,
    Options (..),
    defaultOptions,

    -- * Socket
    ThreadUnsafeSocket (..),
    withThreadUnsafeSocket,
    openThreadUnsafeSocket,
    openThreadSafeSocket,
    setByteStringOption,
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
import Data.Coerce
import Data.Foldable (for_)
import Data.Functor (void, (<&>))
import Data.IORef
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Foreign.C.Types (CInt, CShort)
import GHC.Base (mkWeak#)
import GHC.Exts (keepAlive#)
import GHC.IO (IO (..), unIO)
import GHC.IORef (IORef (..))
import GHC.MVar (MVar (..))
import GHC.STRef (STRef (..))
import GHC.Weak (Weak (..))
import Libzmq
import Libzmq.Bindings qualified
import Numeric.Natural (Natural)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.Weak (deRefWeak)
import System.Posix.Types (Fd (..))
import Zmq.Endpoint
import Zmq.Error (Error, enrichError, enrichFunction, unexpectedError)
import Zmq.Internal (renderEndpoint)

data Context = Context
  { context :: !Zmq_ctx,
    -- A context cannot terminate until all sockets are closed. So, whenever we open a socket, we register a weak
    -- pointer to an action that closes that socket. Sockets thus either close "naturally" (via a finalizer), or during
    -- context termination.
    --
    -- This design allows us to acquire sockets with straight-line syntax, rather than incur a syntactic indent due to
    -- bracketing a resource acquire/release.
    --
    -- FIXME compact this sometimes
    socketClosesRef :: !(IORef [Weak IdempotentSocketClose])
  }

-- This starts out life as a Just zmq_close, and then either:
--
--    1. The socket gets garbage-collected, its finalizer runs, runs the close action contained within, and writes back
--       a Nothing to the ref (needlessly).
--
--    2. The context gets closed, which closes all live sockets, and writes back a Nothing to each ref (so that the
--       finalizer, which will run eventually, won't do anything).
newtype IdempotentSocketClose
  = IdempotentSocketClose (IORef (Maybe (IO ())))

makeIdempotentSocketClose :: Zmq_socket -> IO IdempotentSocketClose
makeIdempotentSocketClose socket =
  coerce (newIORef (Just (void (zmq_close socket))))

runIdempotentSocketClose :: IdempotentSocketClose -> IO ()
runIdempotentSocketClose (IdempotentSocketClose closeRef) =
  readIORef closeRef >>= \case
    Nothing -> pure ()
    Just close -> do
      writeIORef closeRef Nothing
      close

data Options = Options
  { ioThreads :: !Natural,
    maxMessageSize :: !Natural,
    maxSockets :: !Natural
  }

defaultOptions :: Options
defaultOptions =
  Options
    { ioThreads = fromIntegral Libzmq.Bindings._ZMQ_IO_THREADS_DFLT,
      maxMessageSize = fromIntegral @CInt maxBound,
      maxSockets = fromIntegral Libzmq.Bindings._ZMQ_MAX_SOCKETS_DFLT
    }

globalContextRef :: IORef Context
globalContextRef =
  unsafePerformIO (newIORef bogusContext)
{-# NOINLINE globalContextRef #-}

bogusContext :: Context
bogusContext =
  error "zmq library not initialized"

-- | Run a main function.
--
-- This function must be called exactly once, and must wrap all other calls to this library.
run :: Options -> IO a -> IO a
run Options {ioThreads, maxMessageSize, maxSockets} action =
  mask \restore -> do
    context0 <- zmq_ctx_new
    setContextOption context0 ZMQ_IO_THREADS (fromIntegral @Natural @Int ioThreads)
    setContextOption context0 ZMQ_MAX_MSGSZ (fromIntegral @Natural @Int maxMessageSize)
    setContextOption context0 ZMQ_MAX_SOCKETS (fromIntegral @Natural @Int maxSockets)
    socketClosesRef <- newIORef []
    let context =
          Context
            { context = context0,
              socketClosesRef
            }
    writeIORef globalContextRef context
    result <- try (restore action)
    uninterruptibleMask_ (terminateContext context)
    writeIORef globalContextRef bogusContext
    case result of
      Left (exception :: SomeException) -> throwIO exception
      Right value -> pure value

-- Terminate a ØMQ context.
terminateContext :: Context -> IO ()
terminateContext (Context context socketsRef) = do
  -- FIXME we should disallow new sockets from being created here
  sockets <- traverse deRefWeak =<< readIORef socketsRef
  for_ sockets \case
    Nothing -> pure ()
    Just close -> runIdempotentSocketClose close
  let loop = do
        zmq_ctx_term context >>= \case
          Left errno ->
            let err = enrichError "zmq_ctx_term" errno
             in case errno of
                  EFAULT -> throwIO err
                  EINTR -> loop
                  _ -> unexpectedError err
          Right () -> pure ()
  loop

setContextOption :: Zmq_ctx -> Zmq_ctx_option -> Int -> IO ()
setContextOption context option value =
  zmq_ctx_set context option value >>= \case
    Left errno ->
      let err = enrichError "zmq_ctx_set" errno
       in case errno of
            EINVAL -> throwIO err
            _ -> unexpectedError err
    Right () -> pure ()

------------------------------------------------------------------------------------------------------------------------
-- Socket

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
withThreadUnsafeSocket (ThreadUnsafeSocket socket (IORef canary#)) action =
  IO \s0 -> keepAlive# canary# s0 (unIO (action socket))

openThreadUnsafeSocket :: Zmq_socket_type -> IO (Either Error ThreadUnsafeSocket)
openThreadUnsafeSocket =
  openSocket_ \socket -> do
    canary@(IORef (STRef canary#)) <- newIORef ()
    close <- makeIdempotentSocketClose socket
    weak <-
      IO \s0 ->
        case mkWeak# canary# close (unIO (runIdempotentSocketClose close)) s0 of
          (# s1, weak #) -> (# s1, Weak weak #)
    pure (ThreadUnsafeSocket socket canary, weak)

openThreadSafeSocket :: Zmq_socket_type -> IO (Either Error (MVar Zmq_socket))
openThreadSafeSocket =
  openSocket_ \socket -> do
    socketVar@(MVar canary#) <- newMVar socket
    close <- makeIdempotentSocketClose socket
    weak <-
      IO \s0 ->
        case mkWeak# canary# close (unIO (runIdempotentSocketClose close)) s0 of
          (# s1, weak #) -> (# s1, Weak weak #)
    pure (socketVar, weak)

-- FIXME share mkWeak code too?
openSocket_ :: (Zmq_socket -> IO (a, Weak IdempotentSocketClose)) -> Zmq_socket_type -> IO (Either Error a)
openSocket_ oink1 socketType = do
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
        (boo, weak) <- oink1 socket
        atomicModifyIORef' socketsRef \weaks -> (weak : weaks, ())
        pure (Right boo)

setByteStringOption :: Zmq_socket -> CInt -> ByteString -> IO (Either Error ())
setByteStringOption socket option value =
  zmq_setsockopt_bytestring socket option value >>= \case
    Left errno ->
      undefined
        let err = enrichError "zmq_setsockopt" errno
         in case errno of
              EINTR -> setByteStringOption socket option value
              EINVAL -> throwIO err
              ENOTSOCK -> throwIO err
              ETERM -> pure (Left err)
              _ -> unexpectedError err
    Right val -> pure (Right val)

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
