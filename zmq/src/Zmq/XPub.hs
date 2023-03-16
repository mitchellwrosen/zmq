{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zmq.XPub
  ( XPub,
    defaultOptions,
    lossy,
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

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error, catchingOkErrors, enrichError, throwOkError)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll (toPollable), Pollable (..))
import Zmq.Internal.Socket (CanReceive, CanReceives, CanSend, Socket (..))
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __xpublisher__ socket.
--
-- Valid peers: __subscriber__, __xsubscriber__
type XPub =
  Socket "XPUB"

instance Options.CanSetLossy XPub

instance Options.CanSetSendQueueSize XPub

instance CanReceive XPub where
  receive_ = receive

instance CanReceives XPub where
  receives_ = receives

instance CanSend XPub where
  send_ = send

instance CanPoll XPub where
  toPollable Socket {zsocket} =
    PollableNonREQ zsocket

defaultOptions :: Options XPub
defaultOptions =
  Options.defaultOptions

lossy :: Options XPub
lossy =
  Options.lossy

sendQueueSize :: Natural -> Options XPub
sendQueueSize =
  Options.sendQueueSize

-- | Open an __xpublisher__.
open :: Options XPub -> IO (Either Error XPub)
open options =
  catchingOkErrors do
    Socket.openSocket
      ZMQ_XPUB
      ( Options.sockopt ZMQ_RCVHWM 0 -- don't drop subscriptions
          <> Options.sockopt ZMQ_XPUB_NODROP 1 -- not lossy
          <> options
      )
      Socket.XPubExtra

-- | Bind an __xpublisher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: XPub -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind an __xpublisher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: XPub -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect an __xpublisher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: XPub -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect an __xpublisher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: XPub -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on an __xpublisher__ to all peers.
--
-- This operation never blocks:
--
--     * If the 'lossy' option is set, then all peers with full message queues will not receive the message.
--
--     * If the 'lossy' option is not set, and any peer has a full message queue, then the message will not be sent to
--       any peer, and this function will return @EAGAIN@. It is not possible to block until no peer has a full message
--       queue.
--
-- /Alias/: 'Zmq.send'
send :: XPub -> ByteString -> IO (Either Error ())
send socket frame =
  catchingOkErrors do
    sent <- Socket.sendOneDontWait socket frame False
    when (not sent) do
      throwOkError (enrichError "zmq_send" EAGAIN)

-- | Send a __multiframe message__ on an __xpublisher__ to all peers.
--
-- This operation never blocks:
--
--     * If the 'lossy' option is set, then all peers with full message queues will not receive the message.
--
--     * If the 'lossy' option is not set, and any peer has a full message queue, then the message will not be sent to
--       any peer, and this function will return @EAGAIN@. It is not possible to block until no peer has a full message
--       queue.
sends :: XPub -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      sent <- Socket.sendManyDontWait socket (frame :| frames)
      when (not sent) do
        throwOkError (enrichError "zmq_send" EAGAIN)

-- | Receive a __message__ on an __xpublisher__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: XPub -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on an __xpublisher__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: XPub -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
