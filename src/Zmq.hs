module Zmq
  ( BindError
  , ConnectError
  , DisconnectError
  , CanReturnEADDRINUSE
  , CanReturnEADDRNOTAVAIL
  , CanReturnEINVAL
  , CanReturnEMFILE
  , CanReturnEMTHREAD
  , CanReturnENODEV
  , Endpoint(..)
  , Error(..)
  , Function(..)
  , Socket
  , SocketError
  , SocketType(..)
  , Transport(..)
  , bind
  , connect
  , disconnect
  , main
  , socket
  , subscribe
  , unbind
  ) where

import System.Mem (performGC)

import Zmq.API.Bind
import Zmq.API.Connect
import Zmq.API.Disconnect
import Zmq.API.Socket
import Zmq.API.Subscribe
import Zmq.API.Unbind
import Zmq.Context
import Zmq.Error
import Zmq.FFI
import Zmq.Function
import Zmq.Internal
import Zmq.Prelude
import Zmq.Socket


-- | Run an action in the context of a global ZeroMQ context. This should wrap
-- your @main@ function; functions from this library that are called outside of
-- this context will fail at runtime with 'error'.
main :: IO a -> IO a
main =
  bracket_
    ( evaluate context )
    ( fix \again -> do
        performGC -- trigger socket finalizers

        zmq_ctx_term context >>= \case
          0 ->
            pure ()

          _ ->
            zmq_errno >>= \case
              EFAULT_ -> pure ()
              EINTR_  -> again

              n -> errUnexpectedErrno "zmq_ctx_term" n
    )
