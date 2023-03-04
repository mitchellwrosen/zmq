{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Zmq.Internal.SocketFinalizer
  ( SocketFinalizer,
    makeSocketFinalizer,
    runSocketFinalizer,
  )
where

import Data.Coerce
import Data.Functor (void)
import GHC.Base (mkWeak#)
import GHC.Exts (TYPE, UnliftedRep)
import GHC.IO (IO (..), unIO)
import GHC.Weak (Weak (..))
import Libzmq (Zmq_error)
import System.Mem.Weak (deRefWeak)
import Zmq.Internal.Idempotent (Idempotent, makeIdempotent, runIdempotent)

-- | A socket finalizer is a weak reference to an idempotent `zmq_close`.
--
-- Why idempotent: in the case that we explicitly close the socket first (during context teardown), its registered
-- finalizer will still run when the socket is GC'd, and we don't want to call `zmq_close` more than once per socket.
newtype SocketFinalizer
  = SocketFinalizer (Weak (Idempotent ()))

-- pass zmq_close
makeSocketFinalizer :: forall (canary# :: TYPE UnliftedRep). IO (Either Zmq_error ()) -> canary# -> IO SocketFinalizer
makeSocketFinalizer close canary# = do
  idempotentClose <- makeIdempotent (putStrLn "running finalizer" >> void close)
  coerce @(IO (Weak (Idempotent ()))) do
    IO \s0 ->
      case mkWeak# canary# idempotentClose (unIO (runIdempotent idempotentClose)) s0 of
        (# s1, weak #) -> (# s1, Weak weak #)

runSocketFinalizer :: SocketFinalizer -> IO ()
runSocketFinalizer (SocketFinalizer weak) =
  deRefWeak weak >>= \case
    Nothing -> pure ()
    Just close -> runIdempotent close
