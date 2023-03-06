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
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error (..), catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (Socket (withSocket), ThreadSafeSocket)
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
    Options.setSocketOptions socket options
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
send :: Pusher -> ByteString -> IO (Either Error ())
send socket0 message = do
  catchingOkErrors do
    let loop =
          withSocket socket0 \socket ->
            Socket.sendDontWait socket message >>= \case
              False -> do
                Socket.blockUntilCanSend socket
                loop
              True -> pure ()
    loop
