module Zmq.Publisher
  ( Publisher,
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
import Zmq.Internal.Socket (Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __publisher__ socket.
--
-- Valid peers: __subscriber__, __xsubscriber__
newtype Publisher
  = Publisher (MVar Zmq_socket)
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetLossy,
      Options.CanSetSendQueueSize
    )
  deriving (Socket) via (ThreadSafeSocket)

defaultOptions :: Options Publisher
defaultOptions =
  Options.defaultOptions

lossy :: Options Publisher
lossy =
  Options.lossy

sendQueueSize :: Natural -> Options Publisher
sendQueueSize =
  Options.sendQueueSize

-- | Open a __publisher__.
open :: Options Publisher -> IO (Either Error Publisher)
open options =
  catchingOkErrors do
    socketVar <- Socket.openThreadSafeSocket ZMQ_PUB
    socket <- readMVar socketVar
    Options.setSocketOption socket ZMQ_RCVHWM 0 -- don't drop subscriptions
    Options.setSocketOptions socket ZMQ_PUB options
    pure (Publisher socketVar)

-- | Bind a __publisher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Publisher -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __publisher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Publisher -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __publisher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Publisher -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __publisher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Publisher -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __publisher__ to all peers.
--
-- This operation never blocks:
--
--     * If the 'lossy' option is set, then all peers with full message queues will not receive the message.
--
--     * If the 'lossy' option is not set, and any peer has a full message queue, then the message will not be sent to
--       any peer, and this function will return @EAGAIN@. It is not possible to block until no peer has a full message
--       queue.
send :: Publisher -> ByteString -> IO (Either Error ())
send socket0 frame =
  catchingOkErrors do
    withSocket socket0 \socket ->
      Socket.sendDontWait socket frame >>= \case
        True -> pure ()
        False -> throwOkError (enrichError "zmq_send" EAGAIN)

-- | Send a __multiframe message__ on a __publisher__ to all peers.
--
-- This operation never blocks:
--
--     * If the 'lossy' option is set, then all peers with full message queues will not receive the message.
--
--     * If the 'lossy' option is not set, and any peer has a full message queue, then the message will not be sent to
--       any peer, and this function will return @EAGAIN@. It is not possible to block until no peer has a full message
--       queue.
sends :: Publisher -> [ByteString] -> IO (Either Error ())
sends socket0 = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      withSocket socket0 \socket ->
        Socket.sendManyDontWait socket (frame :| frames) >>= \case
          True -> pure ()
          False -> throwOkError (enrichError "zmq_send" EAGAIN)
