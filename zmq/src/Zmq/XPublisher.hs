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

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error, catchingOkErrors, enrichError, throwOkError)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll)
import Zmq.Internal.Socket (CanReceive, CanSend, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __xpublisher__ socket.
--
-- Valid peers: __subscriber__, __xsubscriber__
newtype XPublisher
  = XPublisher (MVar Zmq_socket)
  deriving stock (Eq)
  deriving anyclass
    ( CanPoll,
      Options.CanSetLossy,
      Options.CanSetSendQueueSize
    )
  deriving (Socket) via (ThreadSafeSocket)

instance CanReceive XPublisher where
  receive_ = receive

instance CanSend XPublisher where
  send_ = send

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
    socketVar <- Socket.openThreadSafeSocket ZMQ_XPUB
    socket <- readMVar socketVar
    Options.setSocketOption socket ZMQ_RCVHWM 0 -- don't drop subscriptions
    Options.setSocketOptions socket ZMQ_XPUB options
    pure (XPublisher socketVar)

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
send socket0 frame =
  catchingOkErrors do
    withSocket socket0 \socket ->
      Socket.sendOneDontWait socket frame False >>= \case
        True -> pure ()
        False -> throwOkError (enrichError "zmq_send" EAGAIN)

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
sends socket0 = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      withSocket socket0 \socket ->
        Socket.sendManyDontWait socket (frame :| frames) >>= \case
          True -> pure ()
          False -> throwOkError (enrichError "zmq_send" EAGAIN)

-- | Receive a __message__ on an __xpublisher__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: XPublisher -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on an __xpublisher__ from any peer (fair-queued).
receives :: XPublisher -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- (Socket.receiveMany socket)
    pure (frame : frames)
