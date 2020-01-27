module Zmq
  ( BindError
  , ConnectError
  , DisconnectError
  , SendError
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

  , main

  , pubSocket
  , subSocket

  , bind
  , unbind

  , connect
  , disconnect

  , recv
  , send

  , subscribe
  ) where

import System.Mem (performGC)

import Zmq.API.Bind (BindError, bind)
import Zmq.API.Connect (ConnectError, connect)
import Zmq.API.Disconnect (DisconnectError, disconnect)
import Zmq.API.Recv (recv)
import Zmq.API.Send (SendError, send)
import Zmq.API.Socket (SocketError, pubSocket, subSocket)
import Zmq.API.Subscribe (subscribe)
import Zmq.API.Unbind (unbind)
import Zmq.Context (context)
import Zmq.Endpoint (Endpoint(..))
import Zmq.Error (Error(..), pattern EFAULT_, pattern EINTR_, bugUnexpectedErrno)
import Zmq.Function
import Zmq.Internal (Transport(..))
import Zmq.Prelude
import Zmq.Socket (Socket, SocketType(..))
import qualified Zmq.FFI as FFI


-- | Run an action in the context of a global ZeroMQ context. This should wrap
-- your @main@ function; functions from this library that are called outside of
-- this context will fail at runtime with 'error'.
main :: IO a -> IO a
main =
  bracket_
    ( evaluate context )
    ( fix \again -> do
        performGC -- trigger socket finalizers

        FFI.zmq_ctx_term context >>= \case
          0 ->
            pure ()

          _ ->
            FFI.zmq_errno >>= \case
              EFAULT_ -> pure ()
              EINTR_  -> again

              n -> bugUnexpectedErrno "zmq_ctx_term" n
    )
