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

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll)
import Zmq.Internal.Socket (CanReceive, Socket, ThreadSafeSocket (..))
import Zmq.Internal.Socket qualified as Socket

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
  deriving (Socket) via (ThreadSafeSocket)

instance CanReceive Puller where
  receive_ = receive

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
    socketVar <- Socket.openThreadSafeSocket ZMQ_PULL
    socket <- readMVar socketVar
    Options.setSocketOptions socket ZMQ_PULL options
    pure (Puller (ThreadSafeSocket socketVar (Options.optionsName options)))

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
receives :: Puller -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
