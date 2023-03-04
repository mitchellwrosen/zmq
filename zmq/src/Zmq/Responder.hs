module Zmq.Responder
  ( Responder,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    receive,
    canSend,
    canReceive,
  )
where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty as List (NonEmpty)
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error)
import Zmq.Internal.Socket (CanReceive, CanSend, Event, Socket (withSocket), ThreadUnsafeSocket (..))
import Zmq.Internal.Socket qualified as Socket

-- | A __responder__ socket.
--
-- Valid peers: __requester__, __dealer__
newtype Responder
  = Responder ThreadUnsafeSocket
  deriving stock (Eq)
  deriving newtype (Socket)

instance CanSend Responder

instance CanReceive Responder

-- | Open a __responder__.
open :: IO (Either Error Responder)
open =
  coerce (Socket.openThreadUnsafeSocket ZMQ_REP)

-- | Bind a __responder__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Responder -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __responder__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Responder -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __responder__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Responder -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __responder__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Responder -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __responder__ to the last peer received from.
--
-- This operation never blocks. If the last peer received from no longer exists, the message is discarded.
send :: Responder -> List.NonEmpty ByteString -> IO (Either Error ())
send socket0 message =
  withSocket socket0 \socket -> Socket.send socket message

-- | Receive a __message__ on a __responder__ from any peer (fair queueing).
receive :: Responder -> IO (Either Error (List.NonEmpty ByteString))
receive socket =
  withSocket socket Socket.receive

-- | /Alias/: 'Zmq.canSend'
canSend :: Responder -> a -> Event a
canSend =
  Socket.canSend

-- | /Alias/: 'Zmq.canReceive'
canReceive :: Responder -> a -> Event a
canReceive =
  Socket.canReceive
