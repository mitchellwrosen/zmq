module Zmq.Pub
  ( Pub,
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
import Zmq.Internal.Socket (CanSend, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadSafeSocket (ThreadSafeSocket)
import Zmq.Internal.ThreadSafeSocket qualified as ThreadSafeSocket

-- | A thread-safe __publisher__ socket.
--
-- Valid peers: __subscriber__, __xsubscriber__
newtype Pub
  = Pub ThreadSafeSocket
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetLossy,
      Options.CanSetSendQueueSize
    )

instance CanSend Pub where
  send_ = send

instance Socket Pub where
  openSocket = open
  getSocket = coerce ThreadSafeSocket.raw
  withSocket (Pub socket) = ThreadSafeSocket.with socket
  socketName = coerce ThreadSafeSocket.name

defaultOptions :: Options Pub
defaultOptions =
  Options.defaultOptions

lossy :: Options Pub
lossy =
  Options.lossy

sendQueueSize :: Natural -> Options Pub
sendQueueSize =
  Options.sendQueueSize

-- | Open a __publisher__.
open :: Options Pub -> IO (Either Error Pub)
open options =
  catchingOkErrors do
    coerce do
      ThreadSafeSocket.open
        ZMQ_PUB
        ( Options.sockopt ZMQ_RCVHWM 0 -- don't drop subscriptions
            <> Options.sockopt ZMQ_XPUB_NODROP 1 -- not lossy
            <> options
        )

-- | Bind a __publisher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Pub -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __publisher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Pub -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __publisher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Pub -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __publisher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Pub -> Text -> IO ()
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
--
-- /Alias/: 'Zmq.send'
send :: Pub -> ByteString -> IO (Either Error ())
send socket frame =
  catchingOkErrors do
    sent <- Socket.sendOneDontWait socket frame False
    when (not sent) do
      throwOkError (enrichError "zmq_send" EAGAIN)

-- | Send a __multiframe message__ on a __publisher__ to all peers.
--
-- This operation never blocks:
--
--     * If the 'lossy' option is set, then all peers with full message queues will not receive the message.
--
--     * If the 'lossy' option is not set, and any peer has a full message queue, then the message will not be sent to
--       any peer, and this function will return @EAGAIN@. It is not possible to block until no peer has a full message
--       queue.
sends :: Pub -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      sent <- Socket.sendManyDontWait socket (frame :| frames)
      when (not sent) do
        throwOkError (enrichError "zmq_send" EAGAIN)
