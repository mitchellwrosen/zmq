{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanReceive, CanReceives, Socket (..))
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __subscriber__ socket.
--
-- Valid peers: __publisher__, __xpublisher__
type Sub =
  Socket "SUB"

instance Options.CanSetSendQueueSize Sub

instance CanReceive Sub where
  receive_ = receive

instance CanReceives Sub where
  receives_ = receives

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
    Socket.openSocket
      ZMQ_SUB
      ( Options.sockopt ZMQ_SNDHWM 0 -- don't drop subscriptions
          <> options
      )
      Socket.SubExtra

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
subscribe socket@Socket {zsocket} prefix =
  catchingOkErrors do
    Socket.usingSocket socket do
      Options.setSocketOptions zsocket (Options.sockopt ZMQ_SUBSCRIBE prefix)

-- | Unsubscribe a __subscriber__ from a previously-subscribed __topic__.
unsubscribe :: Sub -> ByteString -> IO (Either Error ())
unsubscribe socket@Socket {zsocket} prefix =
  catchingOkErrors do
    Socket.usingSocket socket do
      Options.setSocketOptions zsocket (Options.sockopt ZMQ_UNSUBSCRIBE prefix)

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
