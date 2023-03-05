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
    canSend,
  )
where

import Control.Concurrent.MVar
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error, catchingOkErrors)
import Zmq.Internal.PublisherOptions (Options (..), defaultOptions, lossy)
import Zmq.Internal.Socket (CanSend, Event, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __publisher__ socket.
--
-- Valid peers: __subscriber__, __xsubscriber__
newtype Publisher
  = Publisher (MVar Zmq_socket)
  deriving stock (Eq)
  deriving (Socket) via (ThreadSafeSocket)

instance CanSend Publisher

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
-- This operation never blocks. If a peer has a full message queue, it will not receive the message.
send :: Publisher -> ByteString -> ByteString -> IO (Either Error ())
send socket0 topic message =
  withSocket socket0 \socket ->
    Socket.send2 socket topic message

-- | /Alias/: 'Zmq.canSend'
canSend :: Publisher -> a -> Event a
canSend =
  Socket.canSend
