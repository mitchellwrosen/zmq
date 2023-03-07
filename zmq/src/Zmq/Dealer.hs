module Zmq.Dealer
  ( Dealer,
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

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error (..), catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanReceive, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __dealer__ socket.
--
-- Valid peers: __dealer__, __replier__, __router__
newtype Dealer
  = Dealer (MVar Zmq_socket)
  deriving stock (Eq)
  deriving anyclass
    ( CanReceive,
      Options.CanSetSendQueueSize
    )
  deriving (Socket) via (ThreadSafeSocket)

defaultOptions :: Options Dealer
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Dealer
sendQueueSize =
  Options.sendQueueSize

-- | Open an __dealer__.
open :: Options Dealer -> IO (Either Error Dealer)
open options =
  catchingOkErrors do
    socketVar <- Socket.openThreadSafeSocket ZMQ_DEALER
    socket <- readMVar socketVar
    Options.setSocketOptions socket ZMQ_DEALER options
    pure (Dealer socketVar)

-- | Bind a __dealer__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Dealer -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __dealer__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Dealer -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __dealer__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Dealer -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __dealer__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Dealer -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __dealer__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
send :: Dealer -> ByteString -> IO (Either Error ())
send socket0 frame =
  catchingOkErrors do
    withSocket socket0 \socket -> do
      let loop =
            Socket.sendOneDontWait socket frame False >>= \case
              True -> pure ()
              False -> do
                Socket.blockUntilCanSend socket
                loop
      loop

-- | Send a __multiframe message__ on a __dealer__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
sends :: Dealer -> [ByteString] -> IO (Either Error ())
sends socket0 = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      withSocket socket0 \socket -> do
        let loop =
              Socket.sendManyDontWait socket (frame :| frames) >>= \case
                True -> pure ()
                False -> do
                  Socket.blockUntilCanSend socket
                  loop
        loop

-- | Receive a __message__ on an __dealer__ from any peer (fair-queued).
receive :: Dealer -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors do
    withSocket socket Socket.receive

-- | Receive a __multiframe message__ on an __dealer__ from any peer (fair-queued).
receives :: Dealer -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- withSocket socket Socket.receiveMany
    pure (frame : frames)
