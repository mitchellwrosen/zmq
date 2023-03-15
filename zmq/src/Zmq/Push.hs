module Zmq.Push
  ( Push,
    defaultOptions,
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
import Zmq.Error (Error (..), catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanSend, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadSafeSocket (ThreadSafeSocket)
import Zmq.Internal.ThreadSafeSocket qualified as ThreadSafeSocket

-- | A thread-safe __pusher__ socket.
--
-- Valid peers: __puller__
newtype Push
  = Push ThreadSafeSocket
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetSendQueueSize
    )

instance CanSend Push where
  send_ = send

instance Socket Push where
  openSocket = open
  getSocket = coerce ThreadSafeSocket.raw
  withSocket (Push socket) = ThreadSafeSocket.with socket
  socketName = coerce ThreadSafeSocket.name

defaultOptions :: Options Push
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Push
sendQueueSize =
  Options.sendQueueSize

-- | Open a __pusher__.
open :: Options Push -> IO (Either Error Push)
open options =
  catchingOkErrors do
    coerce (ThreadSafeSocket.open ZMQ_PUSH options)

-- | Bind a __pusher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Push -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __pusher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Push -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __pusher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Push -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __pusher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Push -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __pusher__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
--
-- /Alias/: 'Zmq.send'
send :: Push -> ByteString -> IO (Either Error ())
send socket frame =
  catchingOkErrors loop
  where
    loop = do
      sent <- Socket.sendOneDontWait socket frame False
      when (not sent) do
        Socket.blockUntilCanSend socket
        loop

-- | Send a __multiframe message__ on a __pusher__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
sends :: Push -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames -> do
    let loop = do
          sent <- Socket.sendManyDontWait socket (frame :| frames)
          when (not sent) do
            Socket.blockUntilCanSend socket
            loop
    catchingOkErrors loop
