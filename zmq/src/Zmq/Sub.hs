module Zmq.Sub
  ( Sub,
    defaultOptions,
    sendQueueSize,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    subscribe,
    unsubscribe,
    receive,
    receives,
  )
where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll (toPollable), Pollable (PollableNonREQ))
import Zmq.Internal.Socket (CanReceive, CanReceives, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadSafeSocket (ThreadSafeSocket)
import Zmq.Internal.ThreadSafeSocket qualified as ThreadSafeSocket

-- | A thread-safe __subscriber__ socket.
--
-- Valid peers: __publisher__, __xpublisher__
newtype Sub
  = Sub (ThreadSafeSocket)
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetSendQueueSize
    )

instance CanPoll Sub where
  toPollable = PollableNonREQ . Socket.getSocket

instance CanReceive Sub where
  receive_ = receive

instance CanReceives Sub where
  receives_ = receives

instance Socket Sub where
  openSocket = open
  getSocket = coerce ThreadSafeSocket.raw
  withSocket (Sub socket) = ThreadSafeSocket.with socket
  socketName = coerce ThreadSafeSocket.name

defaultOptions :: Options Sub
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Sub
sendQueueSize =
  Options.sendQueueSize

-- | Open a __subscriber__.
open :: Options Sub -> IO (Either Error Sub)
open options =
  catchingOkErrors do
    coerce do
      ThreadSafeSocket.open
        ZMQ_SUB
        ( Options.sockopt ZMQ_SNDHWM 0 -- don't drop subscriptions
            <> options
        )

-- | Bind a __subscriber__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Sub -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __subscriber__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Sub -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __subscriber__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Sub -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __subscriber__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Sub -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Subscribe a __subscriber__ to a __topic__ (prefix matching).
--
-- To subscribe to all topics, subscribe to the empty string.
subscribe :: Sub -> ByteString -> IO (Either Error ())
subscribe socket prefix =
  catchingOkErrors do
    withSocket socket do
      Options.setSocketOptions (Socket.getSocket socket) (Options.sockopt ZMQ_SUBSCRIBE prefix)

-- | Unsubscribe a __subscriber__ from a previously-subscribed __topic__.
unsubscribe :: Sub -> ByteString -> IO (Either Error ())
unsubscribe socket prefix =
  catchingOkErrors do
    withSocket socket do
      Options.setSocketOptions (Socket.getSocket socket) (Options.sockopt ZMQ_UNSUBSCRIBE prefix)

-- | Receive a __message__ on a __subscriber__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: Sub -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __subscriber__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: Sub -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
