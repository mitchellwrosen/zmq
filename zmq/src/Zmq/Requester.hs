module Zmq.Requester
  ( Requester,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    receive,
  )
where

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty as List (NonEmpty)
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error)
import Zmq.Internal.Socket (Socket (withSocket), ThreadUnsafeSocket (..))
import Zmq.Internal.Socket qualified as Socket

-- | A __requester__ socket.
--
-- Valid peers: __responder__, __router__
newtype Requester
  = Requester ThreadUnsafeSocket
  deriving stock (Eq)
  deriving newtype (Socket)

-- | Open a __requester__.
open :: IO (Either Error Requester)
open =
  coerce (Socket.openThreadUnsafeSocket ZMQ_REQ)

-- | Bind a __requester__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Requester -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __requester__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Requester -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __requester__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Requester -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __requester__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Requester -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __requester__ to one peer (fair queueing).
--
-- This operation blocks until a peer can receive the message.
send :: Requester -> List.NonEmpty ByteString -> IO (Either Error ())
send socket0 message =
  withSocket socket0 \socket -> Socket.send socket message

-- | Receive a __message__ on a __requester__ from the last peer sent to.
receive :: Requester -> IO (Either Error (List.NonEmpty ByteString))
receive socket =
  withSocket socket Socket.receive
