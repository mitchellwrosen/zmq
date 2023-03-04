module Zmq.Pusher
  ( Pusher,
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
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error)
import Zmq.Internal.Socket (CanSend, Event, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __pusher__ socket.
--
-- Valid peers: __puller__
newtype Pusher
  = Pusher (MVar Zmq_socket)
  deriving stock (Eq)
  deriving (Socket) via (ThreadSafeSocket)

instance CanSend Pusher

-- | Open a __pusher__.
open :: IO (Either Error Pusher)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_PUSH)

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

-- | Send a __message__ on a __pusher__ to one peer (fair queueing).
--
-- This operation blocks until a peer can receive the message.
send :: Pusher -> List.NonEmpty ByteString -> IO (Either Error ())
send socket0 message =
  withSocket socket0 \socket ->
    Socket.send socket message

-- | /Alias/: 'Zmq.canSend'
canSend :: Pusher -> a -> Event a
canSend =
  Socket.canSend
