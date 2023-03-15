module Zmq.Pair
  ( Pair,
    defaultOptions,
    sendQueueSize,
    open,
    open_,
    bind,
    unbind,
    connect,
    connect_,
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
import Zmq.Error (Error (..), catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll (toPollable), Pollable (..))
import Zmq.Internal.Socket (CanReceive, CanReceives, CanSend, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadSafeSocket (ThreadSafeSocket)
import Zmq.Internal.ThreadSafeSocket qualified as ThreadSafeSocket

-- | A thread-safe __pair__ socket.
--
-- Valid peers: __pair__
newtype Pair
  = Pair ThreadSafeSocket
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetSendQueueSize
    )

instance CanPoll Pair where
  toPollable = PollableNonREQ . Socket.getSocket

instance CanReceive Pair where
  receive_ = receive

instance CanReceives Pair where
  receives_ = receives

instance CanSend Pair where
  send_ = send

instance Socket Pair where
  openSocket = open
  getSocket = coerce ThreadSafeSocket.raw
  withSocket (Pair socket) = ThreadSafeSocket.with socket
  socketName = coerce ThreadSafeSocket.name

defaultOptions :: Options Pair
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Pair
sendQueueSize =
  Options.sendQueueSize

-- | Open a __pair__.
open :: Options Pair -> IO (Either Error Pair)
open options =
  catchingOkErrors (open_ options)

open_ :: Options Pair -> IO Pair
open_ options =
  coerce (ThreadSafeSocket.open ZMQ_PAIR options)

-- | Bind a __pair__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Pair -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __pair__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Pair -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __pair__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Pair -> Text -> IO (Either Error ())
connect =
  Socket.connect

connect_ :: Pair -> Text -> IO ()
connect_ =
  Socket.connect_

-- | Disconnect a __pair__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Pair -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __pair__ to the peer.
--
-- This operation blocks until the peer can receive the message.
--
-- /Alias/: 'Zmq.send'
send :: Pair -> ByteString -> IO (Either Error ())
send socket frame =
  catchingOkErrors loop
  where
    loop = do
      sent <- Socket.sendOneDontWait socket frame False
      when (not sent) do
        Socket.blockUntilCanSend socket
        loop

-- | Send a __multiframe message__ on a __pair__ to the peer.
--
-- This operation blocks until the peer can receive the message.
sends :: Pair -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames -> do
    let loop = do
          sent <- Socket.sendManyDontWait socket (frame :| frames)
          when (not sent) do
            Socket.blockUntilCanSend socket
            loop
    catchingOkErrors loop

-- | Receive a __message__ on a __pair__ from the peer.
--
-- /Alias/: 'Zmq.receive'
receive :: Pair -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __pair__ from the peer.
--
-- /Alias/: 'Zmq.receives'
receives :: Pair -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
