module Zmq.Rep
  ( Rep,
    defaultOptions,
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

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error, catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll (toPollable), Pollable (..))
import Zmq.Internal.Socket (CanReceive, CanReceives, CanSend, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadUnsafeSocket (ThreadUnsafeSocket)
import Zmq.Internal.ThreadUnsafeSocket qualified as ThreadUnsafeSocket

-- | A __replier__ socket.
--
-- Valid peers: __dealer__, __requester__
newtype Rep
  = Rep ThreadUnsafeSocket
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetSendQueueSize
    )

instance CanPoll Rep where
  toPollable = PollableNonREQ . Socket.getSocket

instance CanReceive Rep where
  receive_ = receive

instance CanReceives Rep where
  receives_ = receives

instance CanSend Rep where
  send_ = send

instance Socket Rep where
  openSocket = open
  getSocket = coerce ThreadUnsafeSocket.raw
  withSocket (Rep socket) = ThreadUnsafeSocket.with socket
  socketName = coerce ThreadUnsafeSocket.name

defaultOptions :: Options Rep
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Rep
sendQueueSize =
  Options.sendQueueSize

-- | Open a __replier__.
open :: Options Rep -> IO (Either Error Rep)
open options =
  catchingOkErrors do
    coerce (ThreadUnsafeSocket.open ZMQ_REP options)

-- | Bind a __replier__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Rep -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __replier__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Rep -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __replier__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Rep -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __replier__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Rep -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __replier__ to the last peer received from.
--
-- If the last peer received from no longer exists, the message is discarded.
--
-- /Alias/: 'Zmq.send'
send :: Rep -> ByteString -> IO (Either Error ())
send socket frame =
  catchingOkErrors do
    Socket.sendOneWontBlock socket frame False

-- | Send a __multiframe message__ on a __replier__ to the last peer received from.
--
-- If the last peer received from no longer exists, the message is discarded.
sends :: Rep -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      Socket.sendManyWontBlock socket (frame :| frames)

-- | Receive a __message__ on a __replier__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: Rep -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __replier__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: Rep -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
