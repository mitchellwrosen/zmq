module Zmq.Router
  ( Router,
    defaultOptions,
    sendQueueSize,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    sends,
    receive,
    receives,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty as List (NonEmpty)
import Data.List.NonEmpty as List.NonEmpty
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error, catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanReceive, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __router__ socket.
--
-- Valid peers: __dealer__, __requester__, __router__
newtype Router
  = Router (MVar Zmq_socket)
  deriving stock (Eq)
  deriving (Socket) via (ThreadSafeSocket)
  deriving anyclass
    ( CanReceive,
      Options.CanSetSendQueueSize
    )

defaultOptions :: Options Router
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Router
sendQueueSize =
  Options.sendQueueSize

-- | Open a __router__.
open :: Options Router -> IO (Either Error Router)
open options =
  catchingOkErrors do
    socketVar <- Socket.openThreadSafeSocket ZMQ_ROUTER
    socket <- readMVar socketVar
    Options.setSocketOption socket ZMQ_ROUTER_MANDATORY 1
    Options.setSocketOptions socket options
    pure (Router socketVar)

-- | Bind a __router__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Router -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __router__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Router -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __router__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Router -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __router__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Router -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __routed message__ on a __router__ to a peer.
--
-- If the peer no longer exists, returns @EHOSTUNREACH@.
send :: Router -> ByteString -> ByteString -> IO (Either Error ())
send socket0 identity message =
  catchingOkErrors do
    withSocket socket0 \socket ->
      -- First try a non-blocking send, but if that doesn't work, try a blocking send. We'll get EAGAIN if the peer we
      -- are we are trying to send to has reached its high-water mark. In this case, waiting for the socket to become
      -- writable is not useful for a router - we want to block until we can send to *this* peer. So we do that with
      -- a safe FFI call to zmq_send without ZMQ_DONTWAIT.
      Socket.sendTwoDontWait socket identity message >>= \case
        True -> pure ()
        False -> Socket.sendTwo socket identity message

-- | Send a __routed multiframe message__ on a __router__ to a peer.
--
-- If the peer no longer exists, returns @EHOSTUNREACH@.
sends :: Router -> ByteString -> List.NonEmpty ByteString -> IO (Either Error ())
sends socket0 identity message =
  catchingOkErrors do
    withSocket socket0 \socket ->
      -- See above comment
      Socket.sendManyDontWait socket message1 >>= \case
        True -> pure ()
        False -> Socket.sendMany socket message1
  where
    message1 =
      List.NonEmpty.cons identity message

-- | Receive a __routed message__ on an __router__ from any peer (fair-queued).
receive :: Router -> IO (Either Error (ByteString, ByteString))
receive socket0 =
  catchingOkErrors do
    withSocket socket0 \socket -> do
      identity :| message0 <- Socket.receiveMany socket
      pure
        ( identity,
          case message0 of
            [] -> ByteString.empty
            message : _ -> message
        )

-- | Receive a __routed multiframe message__ on an __router__ from any peer (fair-queued).
receives :: Router -> IO (Either Error (ByteString, List.NonEmpty ByteString))
receives socket0 =
  catchingOkErrors do
    withSocket socket0 \socket -> do
      identity :| message0 <- Socket.receiveMany socket
      pure
        ( identity,
          case message0 of
            [] -> ByteString.empty :| []
            frame : frames -> frame :| frames
        )
