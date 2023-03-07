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

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error (..), catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanSend, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __pusher__ socket.
--
-- Valid peers: __puller__
newtype Pusher
  = Pusher (MVar Zmq_socket)
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetSendQueueSize
    )
  deriving (Socket) via (ThreadSafeSocket)

instance CanSend Pusher where
  send_ = send

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
    socketVar <- Socket.openThreadSafeSocket ZMQ_PUSH
    socket <- readMVar socketVar
    Options.setSocketOptions socket ZMQ_PUSH options
    pure (Pusher socketVar)

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
send socket0 frame = do
  catchingOkErrors do
    withSocket socket0 \socket -> do
      let loop =
            Socket.sendOneDontWait socket frame False >>= \case
              False -> do
                Socket.blockUntilCanSend socket
                loop
              True -> pure ()
      loop

-- | Send a __multiframe message__ on a __pusher__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
sends :: Pusher -> [ByteString] -> IO (Either Error ())
sends socket0 = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      withSocket socket0 \socket -> do
        let loop =
              Socket.sendManyDontWait socket (frame :| frames) >>= \case
                False -> do
                  Socket.blockUntilCanSend socket
                  loop
                True -> pure ()
        loop
