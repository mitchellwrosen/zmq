module Zmq.Pull
  ( Pull,
    defaultOptions,
    sendQueueSize,
    open,
    bind,
    unbind,
    connect,
    disconnect,
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
import Zmq.Internal.Poll (CanPoll (toPollable), Pollable (..))
import Zmq.Internal.Socket (CanReceive, CanReceives, Socket)
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadSafeSocket (ThreadSafeSocket)
import Zmq.Internal.ThreadSafeSocket qualified as ThreadSafeSocket

-- | A thread-safe __puller__ socket.
--
-- Valid peers: __pusher__
newtype Pull
  = Pull ThreadSafeSocket
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetSendQueueSize
    )

instance CanPoll Pull where
  toPollable = PollableNonREQ . Socket.getSocket

instance CanReceive Pull where
  receive_ = receive

instance CanReceives Pull where
  receives_ = receives

instance Socket Pull where
  openSocket = open
  getSocket = coerce ThreadSafeSocket.raw
  withSocket (Pull socket) = ThreadSafeSocket.with socket
  socketName = coerce ThreadSafeSocket.name

defaultOptions :: Options Pull
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Pull
sendQueueSize =
  Options.sendQueueSize

-- | Open a __puller__.
open :: Options Pull -> IO (Either Error Pull)
open options =
  catchingOkErrors do
    coerce (ThreadSafeSocket.open ZMQ_PULL options)

-- | Bind a __puller__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Pull -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __puller__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Pull -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __puller__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Pull -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __puller__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Pull -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Receive a __message__ on a __puller__ from one peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: Pull -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __puller__ from one peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: Pull -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
