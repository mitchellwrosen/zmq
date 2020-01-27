module Zmq
  ( BindError
  , ConnectError
  , DisconnectError
  , SendError
  , CanReturnEADDRINUSE
  , CanReturnEADDRNOTAVAIL
  , CanReturnEINVAL
  , CanReturnEMTHREAD
  , CanReturnENODEV
  , Endpoint(..)
  , Error(..)
  , Function(..)
  , Socket
  , SocketType(..)
  , Transport(..)

  , main
  , Options(..)
  , defaultOptions

  , pubSocket
  , subSocket
  , close

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
import Zmq.API.Close (close)
import Zmq.API.Connect (ConnectError, connect)
import Zmq.API.Disconnect (DisconnectError, disconnect)
import Zmq.API.Recv (recv)
import Zmq.API.Send (SendError, send)
import Zmq.API.Socket (pubSocket, subSocket)
import Zmq.API.Subscribe (subscribe)
import Zmq.API.Unbind (unbind)
import Zmq.Context (context, setMaxSockets)
import Zmq.Endpoint (Endpoint(..))
import Zmq.Error (Error(..), pattern EINTR_, bugUnexpectedErrno)
import Zmq.Function
import Zmq.Internal (Transport(..))
import Zmq.Prelude
import Zmq.Socket (Socket, SocketType(..))
import qualified Zmq.FFI as FFI


data Options
  = Options
  { maxSockets :: Natural
  }

defaultOptions :: Options
defaultOptions =
  Options
    { maxSockets = fromIntegral FFI.zMQ_MAX_SOCKETS_DFLT
    }

-- | Run an action in the context of a global ZeroMQ context. This should wrap
-- your @main@ function, and must only be called once; functions from this
-- library that are called outside of this context will fail at runtime.
main :: Options -> IO a -> IO a
main options action =
  bracket_ setup teardown do
    setMaxSockets ( maxSockets options )
    action

  where
    setup =
      evaluate context

    teardown =
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
                bugUnexpectedErrno "zmq_ctx_term" errno
