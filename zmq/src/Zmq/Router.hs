module Zmq.Router
  ( Router,
    defaultOptions,
    sendQueueSize,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    sends,
    receives,
  )
where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error, catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll (toPollable), Pollable (..))
import Zmq.Internal.Socket (CanReceives, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadSafeSocket (ThreadSafeSocket)
import Zmq.Internal.ThreadSafeSocket qualified as ThreadSafeSocket

-- | A thread-safe __router__ socket.
--
-- Valid peers: __dealer__, __requester__, __router__
newtype Router
  = Router (ThreadSafeSocket)
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetSendQueueSize
    )

instance CanPoll Router where
  toPollable = PollableNonREQ . Socket.getSocket

instance CanReceives Router where
  receives_ = receives

instance Socket Router where
  openSocket = open
  getSocket = coerce ThreadSafeSocket.raw
  withSocket (Router socket) = ThreadSafeSocket.with socket
  socketName = coerce ThreadSafeSocket.name

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
    coerce (ThreadSafeSocket.open ZMQ_ROUTER (Options.sockopt ZMQ_ROUTER_MANDATORY 1 <> options))

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

-- | Send a __multiframe message__ on a __router__ to a peer.
--
-- If the peer no longer exists, returns @EHOSTUNREACH@.
sends :: Router -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames -> do
    let message = frame :| frames
    catchingOkErrors do
      -- First try a non-blocking send, but if that doesn't work, try a blocking send. We'll get EAGAIN if the peer we
      -- are we are trying to send to has reached its high-water mark. In this case, waiting for the socket to become
      -- writable is not useful for a router - we want to block until we can send to *this* peer. So we do that with a
      -- safe FFI call to zmq_send without ZMQ_DONTWAIT. Note that this means while we're blocking in send, other
      -- threads can't receive on this router.
      Socket.sendManyDontWait socket message >>= \case
        True -> pure ()
        False -> Socket.sendMany socket message

-- | Receive a __multiframe message__ on a __router__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: Router -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
