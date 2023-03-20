{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanReceive, CanReceives, Socket (..))
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __puller__ socket.
--
-- Valid peers: __pusher__
type Pull =
  Socket "PULL"

instance Options.CanSetSendQueueSize Pull

instance CanReceive Pull where
  receive_ = receive

instance CanReceives Pull where
  receives_ = receives

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
    Socket.openSocket ZMQ_PULL options Socket.PullExtra

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
