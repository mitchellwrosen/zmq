module Zmq.Puller
  ( Puller,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    receive,
    canReceive,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty as List (NonEmpty)
import Data.Text (Text)
import Libzmq
import Zmq.Error
import Zmq.Internal.Socket (CanReceive, Event, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __puller__ socket.
--
-- Valid peers: __pusher__
newtype Puller
  = Puller (MVar Zmq_socket)
  deriving stock (Eq)
  deriving (Socket) via (ThreadSafeSocket)

instance CanReceive Puller

-- | Open a __puller__.
open :: IO (Either Error Puller)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_PULL)

-- | Bind a __puller__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Puller -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __puller__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Puller -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __puller__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Puller -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __puller__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Puller -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Receive a __message__ on a __puller__ from one peer (fair-queued).
receive :: Puller -> IO (Either Error (List.NonEmpty ByteString))
receive socket =
  withSocket socket Socket.receive

-- | /Alias/: 'Zmq.canReceive'
canReceive :: Puller -> a -> Event a
canReceive =
  Socket.canReceive
