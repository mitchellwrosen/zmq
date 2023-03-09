module Zmq.XPublisher
  ( XPublisher,
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
import Data.Coerce (coerce)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error, catchingOkErrors, enrichError, throwOkError)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll (getSocketType))
import Zmq.Internal.Socket (CanReceive, CanReceives, CanSend, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadSafeSocket (ThreadSafeSocket)
import Zmq.Internal.ThreadSafeSocket qualified as ThreadSafeSocket

-- | A thread-safe __xpublisher__ socket.
--
-- Valid peers: __subscriber__, __xsubscriber__
newtype XPublisher
  = XPublisher ThreadSafeSocket
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetLossy,
      Options.CanSetSendQueueSize
    )

instance CanPoll XPublisher where
  getSocketType = ZMQ_XPUB

instance CanReceive XPublisher where
  receive_ = receive

instance CanReceives XPublisher where
  receives_ = receives

instance CanSend XPublisher where
  send_ = send

instance Socket XPublisher where
  openSocket = open
  getSocket = coerce ThreadSafeSocket.raw
  withSocket (XPublisher socket) = ThreadSafeSocket.with socket
  socketName = coerce ThreadSafeSocket.name

defaultOptions :: Options XPublisher
defaultOptions =
  Options.defaultOptions

lossy :: Options XPublisher
lossy =
  Options.lossy

sendQueueSize :: Natural -> Options XPublisher
sendQueueSize =
  Options.sendQueueSize

-- | Open an __xpublisher__.
open :: Options XPublisher -> IO (Either Error XPublisher)
open options =
  catchingOkErrors do
    coerce do
      ThreadSafeSocket.open
        ZMQ_XPUB
        ( Options.sockopt ZMQ_RCVHWM 0 -- don't drop subscriptions
            <> Options.sockopt ZMQ_XPUB_NODROP 1 -- not lossy
            <> options
        )

-- | Bind an __xpublisher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: XPublisher -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind an __xpublisher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: XPublisher -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect an __xpublisher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: XPublisher -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect an __xpublisher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: XPublisher -> Text -> IO ()
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
send :: XPublisher -> ByteString -> IO (Either Error ())
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
sends :: XPublisher -> [ByteString] -> IO (Either Error ())
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
receive :: XPublisher -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on an __xpublisher__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: XPublisher -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
