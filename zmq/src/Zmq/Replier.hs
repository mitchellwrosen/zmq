module Zmq.Replier
  ( Replier,
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
import Zmq.Internal.Poll (CanPoll)
import Zmq.Internal.Socket (CanReceive, CanReceives, CanSend, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadUnsafeSocket (ThreadUnsafeSocket)
import Zmq.Internal.ThreadUnsafeSocket qualified as ThreadUnsafeSocket

-- | A __replier__ socket.
--
-- Valid peers: __dealer__, __requester__
newtype Replier
  = Replier ThreadUnsafeSocket
  deriving stock (Eq)
  deriving anyclass
    ( CanPoll,
      Options.CanSetSendQueueSize
    )

instance CanReceive Replier where
  receive_ = receive

instance CanReceives Replier where
  receives_ = receives

instance CanSend Replier where
  send_ = send

instance Socket Replier where
  openSocket = open
  getSocket = coerce ThreadUnsafeSocket.raw
  withSocket (Replier socket) = ThreadUnsafeSocket.with socket
  socketName = coerce ThreadUnsafeSocket.name

defaultOptions :: Options Replier
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Replier
sendQueueSize =
  Options.sendQueueSize

-- | Open a __replier__.
open :: Options Replier -> IO (Either Error Replier)
open options =
  catchingOkErrors do
    coerce (ThreadUnsafeSocket.open ZMQ_REP options)

-- | Bind a __replier__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Replier -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __replier__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Replier -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __replier__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Replier -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __replier__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Replier -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __replier__ to the last peer received from.
--
-- If the last peer received from no longer exists, the message is discarded.
--
-- /Alias/: 'Zmq.send'
send :: Replier -> ByteString -> IO (Either Error ())
send socket0 frame =
  catchingOkErrors do
    withSocket socket0 \socket ->
      Socket.sendOneWontBlock socket (Socket.socketName socket0) frame False

-- | Send a __multiframe message__ on a __replier__ to the last peer received from.
--
-- If the last peer received from no longer exists, the message is discarded.
sends :: Replier -> [ByteString] -> IO (Either Error ())
sends socket0 = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      withSocket socket0 \socket ->
        Socket.sendManyWontBlock socket (Socket.socketName socket0) (frame :| frames)

-- | Receive a __message__ on a __replier__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receive'
receive :: Replier -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __replier__ from any peer (fair-queued).
--
-- /Alias/: 'Zmq.receives'
receives :: Replier -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
