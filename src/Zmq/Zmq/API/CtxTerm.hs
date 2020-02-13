module Zmq.API.CtxTerm
  ( ctxTerm
  ) where

import System.Mem (performGC)

import qualified Libzmq
import qualified Zmq.FFI as FFI

import Zmq.Error
import Zmq.Exception
import Zmq.Prelude


ctxTerm
  :: Ptr Libzmq.Context
  -> IO ()
ctxTerm context =
  fix \again -> do
    performGC -- trigger socket finalizers

    Libzmq.terminateContext context >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= \case
          EINTR ->
            again

          errno ->
            exception "zmq_ctx_term" errno

