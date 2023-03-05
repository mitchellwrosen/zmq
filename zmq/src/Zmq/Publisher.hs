module Zmq.Publisher
  ( Publisher,
    Options,
    defaultOptions,
    lossy,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    send,
  )
where

import Control.Concurrent.MVar
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error, catchingOkErrors, enrichError, throwOkError)
import Zmq.Internal.PublisherOptions (Options (..), defaultOptions, lossy)
import Zmq.Internal.Socket (Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __publisher__ socket.
--
-- Valid peers: __subscriber__, __xsubscriber__
newtype Publisher
  = Publisher (MVar Zmq_socket)
  deriving stock (Eq)
  deriving (Socket) via (ThreadSafeSocket)

-- | Open a __publisher__.
open :: Options -> IO (Either Error Publisher)
open Options {lossy_} =
  catchingOkErrors do
    socketVar <- Socket.openThreadSafeSocket ZMQ_PUB
    socket <- readMVar socketVar
    when lossy_ (Socket.setOption socket ZMQ_XPUB_NODROP 1)
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

-- | Send a __topic message__ on a __publisher__ to all peers.
--
-- This operation never blocks:
--
--     * If the 'lossy' option is set, then all peers with full message queues will not receive the message.
--     * Otherwise, if the 'lossy' option is not set, and any peer has a full message queue, then the message will not
--       be sent to any peer, and this function will return @EAGAIN@.
send :: Publisher -> ByteString -> ByteString -> IO (Either Error ())
send socket0 topic message =
  catchingOkErrors do
    withSocket socket0 \socket ->
      Socket.sendTwoDontWait socket topic message >>= \case
        True -> pure ()
        False -> throwOkError (enrichError "zmq_send" EAGAIN)
