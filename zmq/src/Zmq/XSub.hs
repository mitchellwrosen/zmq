{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zmq.XSub
  ( XSub,
    defaultOptions,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    subscribe,
    unsubscribe,
    send,
    sends,
    receive,
    receives,
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error, catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll (toPollable), Pollable (PollableNonREQ))
import Zmq.Internal.Socket (CanReceive, CanReceives, CanSend, Socket (..))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Subscription (pattern Subscribe, pattern Unsubscribe)

-- | A thread-safe __xsubscriber__ socket.
--
-- Valid peers: __publisher__, __xpublisher__
type XSub =
  Socket "XSUB"

instance CanSend XSub where
  send_ = send

instance CanReceive XSub where
  receive_ = receive

instance CanReceives XSub where
  receives_ = receives

instance CanPoll XSub where
  toPollable Socket {zsocket} =
    PollableNonREQ zsocket

defaultOptions :: Options XSub
defaultOptions =
  Options.defaultOptions

-- | Open an __xsubscriber__.
open :: Options XSub -> IO (Either Error XSub)
open options =
  catchingOkErrors do
    Socket.openSocket
      ZMQ_XSUB
      ( Options.sockopt ZMQ_SNDHWM 0 -- don't drop subscriptions
          <> options
      )
      Socket.XSubExtra

-- | Bind an __xsubscriber__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: XSub -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind an __xsubscriber__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: XSub -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect an __xsubscriber__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: XSub -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect an __xsubscriber__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: XSub -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Subscribe an __xsubscriber__ to a __topic__ (prefix matching).
--
-- To subscribe to all topics, subscribe to the empty string.
subscribe :: XSub -> ByteString -> IO (Either Error ())
subscribe socket prefix =
  send socket (Subscribe prefix)

-- | Unsubscribe an __xsubscriber__ from a previously-subscribed __topic__.
unsubscribe :: XSub -> ByteString -> IO (Either Error ())
unsubscribe socket prefix =
  send socket (Unsubscribe prefix)

-- | Send a __message__ on an __xsubscriber__ to all peers.
--
-- This operation never blocks. All peers with full messages queues will not receive the message.
--
-- /Alias/: 'Zmq.send'
send :: XSub -> ByteString -> IO (Either Error ())
send socket frame =
  catchingOkErrors do
    Socket.sendOneWontBlock socket frame False

-- | Send a __multiframe message__ on an __xsubscriber__ to all peers.
--
-- This operation never blocks. All peers with full messages queues will not receive the message.
sends :: XSub -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      Socket.sendManyWontBlock socket (frame :| frames)

-- | Receive a __message__ on an __xsubscriber__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: XSub -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on an __xsubscriber__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: XSub -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
