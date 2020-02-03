module Zmq.API.CtxTerm
  ( ctxTerm
  ) where

import System.Mem (performGC)

import Zmq.Error
import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.FFI as FFI


ctxTerm
  :: Ptr FFI.Context
  -> IO ()
ctxTerm context =
  fix \again -> do
    performGC -- trigger socket finalizers

    FFI.zmq_ctx_term context >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= \case
          EINTR_ ->
            again

          errno ->
            exception "zmq_ctx_term" errno

