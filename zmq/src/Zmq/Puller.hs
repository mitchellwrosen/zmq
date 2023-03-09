module Zmq.Puller
  ( Puller,
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
import Zmq.Internal.Poll (CanPoll)
import Zmq.Internal.Socket (CanReceive, CanReceives, Socket)
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadSafeSocket (ThreadSafeSocket)
import Zmq.Internal.ThreadSafeSocket qualified as ThreadSafeSocket

-- | A thread-safe __puller__ socket.
--
-- Valid peers: __pusher__
newtype Puller
  = Puller ThreadSafeSocket
  deriving stock (Eq)
  deriving anyclass
    ( CanPoll,
      Options.CanSetSendQueueSize
    )

instance CanReceive Puller where
  receive_ = receive

instance CanReceives Puller where
  receives_ = receives

instance Socket Puller where
  openSocket = open
  getSocket = coerce ThreadSafeSocket.raw
  withSocket (Puller socket) = ThreadSafeSocket.with socket
  socketName = coerce ThreadSafeSocket.name

defaultOptions :: Options Puller
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Puller
sendQueueSize =
  Options.sendQueueSize

-- | Open a __puller__.
open :: Options Puller -> IO (Either Error Puller)
open options =
  catchingOkErrors do
    coerce (ThreadSafeSocket.open ZMQ_PULL options)

-- | Bind a __puller__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Puller -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __puller__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Puller -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __puller__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Puller -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __puller__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Puller -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Receive a __message__ on a __puller__ from one peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: Puller -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __puller__ from one peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: Puller -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
