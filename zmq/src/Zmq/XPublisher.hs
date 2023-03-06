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
    receive,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error, catchingOkErrors, enrichError, throwOkError)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanReceive, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __xpublisher__ socket.
--
-- Valid peers: __subscriber__, __xsubscriber__
newtype XPublisher
  = XPublisher (MVar Zmq_socket)
  deriving stock (Eq)
  deriving anyclass
    ( CanReceive,
      Options.CanSetLossy,
      Options.CanSetSendQueueSize
    )
  deriving (Socket) via (ThreadSafeSocket)

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

-- | Send a __topic message__ on an __xpublisher__ to all peers.
--
-- This operation never blocks:
--
--     * If the 'lossy' option is set, then all peers with full message queues will not receive the message.
--     * Otherwise, if the 'lossy' option is not set, and any peer has a full message queue, then the message will not
--       be sent to any peer, and this function will return @EAGAIN@.
send :: XPublisher -> ByteString -> ByteString -> IO (Either Error ())
send socket0 topic message =
  catchingOkErrors do
    withSocket socket0 \socket ->
      Socket.sendTwoDontWait socket topic message >>= \case
        True -> pure ()
        False -> throwOkError (enrichError "zmq_send" EAGAIN)

-- | Receive a __message__ on an __xpublisher__ from any peer (fair-queued).
receive :: XPublisher -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors do
    withSocket socket Socket.receive
