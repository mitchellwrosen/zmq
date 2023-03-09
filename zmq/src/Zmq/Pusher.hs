module Zmq.Pusher
  ( Pusher,
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
newtype Pusher
  = Pusher ThreadSafeSocket
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetSendQueueSize
    )

instance CanSend Pusher where
  send_ = send

instance Socket Pusher where
  openSocket = open
  getSocket = coerce ThreadSafeSocket.raw
  withSocket (Pusher socket) = ThreadSafeSocket.with socket
  socketName = coerce ThreadSafeSocket.name

defaultOptions :: Options Pusher
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Pusher
sendQueueSize =
  Options.sendQueueSize

-- | Open a __pusher__.
open :: Options Pusher -> IO (Either Error Pusher)
open options =
  catchingOkErrors do
    coerce (ThreadSafeSocket.open ZMQ_PUSH options)

-- | Bind a __pusher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Pusher -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __pusher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Pusher -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __pusher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Pusher -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __pusher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Pusher -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __pusher__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
--
-- /Alias/: 'Zmq.send'
send :: Pusher -> ByteString -> IO (Either Error ())
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
sends :: Pusher -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames -> do
    let loop = do
          sent <- Socket.sendManyDontWait socket (frame :| frames)
          when (not sent) do
            Socket.blockUntilCanSend socket
            loop
    catchingOkErrors loop
