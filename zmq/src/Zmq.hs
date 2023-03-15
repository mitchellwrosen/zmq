module Zmq
  ( -- * Main
    run,

    -- ** Main options
    ioThreads,
    maxSockets,

    -- * Socket
    Socket,
    open,
    monitor,

    -- ** Options
    curveClient,
    curveServer,
    lossy,
    name,
    sendQueueSize,

    -- ** Peering
    bind,
    unbind,
    connect,
    disconnect,

    -- ** Messaging
    send,
    receive,
    receives,

    -- ** IO multiplexing
    Sockets,
    the,
    also,
    poll,
    pollFor,
    pollUntil,

    -- * Socket types
    Dealer,
    Pair,
    Pub,
    Pull,
    Push,
    Rep,
    Req,
    Router,
    Sub,
    XPub,
    XSub,

    -- * Subscription message
    pattern Subscribe,
    pattern Unsubscribe,

    -- * Encryption
    CurvePublicKey (..),
    CurveSecretKey (..),
    generateCurveSecretKey,
    deriveCurvePublicKey,

    -- * Options
    Options,
    defaultOptions,

    -- * Errors
    Error (..),
    Zmq_error (..),

    -- * Socket subclasses
    CanSend,
    CanReceive,
    CanReceives,
    CanPoll,

    -- ** Options
    CanSetLossy,
    CanSetSendQueueSize,

    -- * Version
    version,
  )
where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.ByteString.Short.Base64.URL qualified as Base64
import Data.Text.Short qualified as Text.Short
import Libzmq
import System.Random.Stateful qualified as Random
import Zmq.Dealer (Dealer)
import Zmq.Error (Error (..), catchingOkErrors, enrichError, throwOkError, unexpectedError)
import Zmq.Internal.Context
import Zmq.Internal.Curve
import Zmq.Internal.Options
  ( CanSetLossy,
    CanSetSendQueueSize,
    Options,
    curveClient,
    curveServer,
    defaultOptions,
    ioThreads,
    lossy,
    maxSockets,
    name,
    sendQueueSize,
  )
import Zmq.Internal.Poll (CanPoll, Sockets, also, poll, pollFor, pollUntil, the)
import Zmq.Internal.Socket
  ( CanReceive (receive_),
    CanReceives,
    CanSend (send_),
    Socket (openSocket),
    bind,
    connect,
    disconnect,
    receives_,
    unbind,
  )
import Zmq.Internal.Socket qualified as Socket
import Zmq.Pair (Pair)
import Zmq.Pair qualified as Pair
import Zmq.Pub (Pub)
import Zmq.Pull (Pull)
import Zmq.Push (Push)
import Zmq.Rep (Rep)
import Zmq.Req (Req)
import Zmq.Router (Router)
import Zmq.Sub (Sub)
import Zmq.Subscription (pattern Subscribe, pattern Unsubscribe)
import Zmq.XPub (XPub)
import Zmq.XSub (XSub)

open :: Socket socket => Options socket -> IO (Either Error socket)
open =
  openSocket

monitor :: Socket socket => socket -> IO (Either Error Pair)
monitor socket = do
  endpointBytes <- Random.uniformShortByteString 16 Random.globalStdGen
  let endpoint = Text.Short.toText ("inproc://" <> Base64.encodeBase64Unpadded endpointBytes)
  catchingOkErrors do
    zmq_socket_monitor (Socket.getSocket socket) endpoint ZMQ_EVENT_ALL >>= \case
      Left errno ->
        let err = enrichError "zmq_socket_monitor" errno
         in case errno of
              EINVAL -> throwIO err
              ETERM -> throwOkError err
              EPROTONOSUPPORT -> throwIO err
              _ -> unexpectedError err
      Right () -> do
        pair <- Pair.open_ (name (Socket.socketName socket <> "-monitor"))
        Pair.connect_ pair endpoint
        pure pair

send :: CanSend socket => socket -> ByteString -> IO (Either Error ())
send =
  send_

receive :: CanReceive socket => socket -> IO (Either Error ByteString)
receive =
  receive_

receives :: CanReceives socket => socket -> IO (Either Error [ByteString])
receives =
  receives_

version :: (Int, Int, Int)
version =
  zmq_version
