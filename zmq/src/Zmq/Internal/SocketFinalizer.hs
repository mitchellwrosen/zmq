{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Zmq.Internal.SocketFinalizer
  ( SocketFinalizer,
    makeSocketFinalizer,
    runSocketFinalizer,
  )
where

import Data.Functor (void)
import Data.IORef
import GHC.Base (mkWeak#)
import GHC.Exts (TYPE, UnliftedRep)
import GHC.IO (IO (..), unIO)
import GHC.Weak (Weak (..))
import Libzmq (Zmq_error, Zmq_socket_option (ZMQ_LINGER))
import System.Mem.Weak (deRefWeak)

-- | A socket finalizer is a weak reference to an idempotent linger+close action.
--
-- Why idempotent: in the case that we explicitly close the socket first (during context teardown), its registered
-- finalizer will still run when the socket is GC'd, and we don't want to finalize more than once per socket.
newtype SocketFinalizer
  = SocketFinalizer (Weak (IO ()))

makeSocketFinalizer ::
  forall (canary# :: TYPE UnliftedRep).
  -- zmq_setsockopt
  (forall a. Zmq_socket_option a -> a -> IO (Either Zmq_error ())) ->
  -- zmq_close
  IO (Either Zmq_error ()) ->
  canary# ->
  IO SocketFinalizer
makeSocketFinalizer setsockopt close canary# = do
  idempotentFinalize <-
    makeIdempotent do
      void (setsockopt ZMQ_LINGER 1_000) -- linger 1 second
      void close
  weak <- makeWeakPointer canary# idempotentFinalize idempotentFinalize
  pure (SocketFinalizer weak)

makeWeakPointer :: forall (key# :: TYPE UnliftedRep) value. key# -> value -> IO () -> IO (Weak value)
makeWeakPointer key# value finalizer =
  IO \s0 ->
    case mkWeak# key# value (unIO finalizer) s0 of
      (# s1, weak #) -> (# s1, Weak weak #)

runSocketFinalizer :: SocketFinalizer -> IO ()
runSocketFinalizer (SocketFinalizer weak) =
  deRefWeak weak >>= \case
    Nothing -> pure ()
    Just close -> close

makeIdempotent :: IO a -> IO (IO a)
makeIdempotent action = do
  resultRef <- newIORef Nothing
  pure do
    readIORef resultRef >>= \case
      Nothing -> do
        result <- action
        writeIORef resultRef (Just result)
        pure result
      Just result -> pure result
