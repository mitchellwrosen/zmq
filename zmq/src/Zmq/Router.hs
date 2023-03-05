module Zmq.Router
  ( Router,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    sends,
    receive,
    receives,
    canSend,
    canReceive,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Functor ((<&>))
import Data.List.NonEmpty as List (NonEmpty)
import Data.List.NonEmpty as List.NonEmpty
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error)
import Zmq.Internal.Socket (CanReceive, CanSend, Event, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __router__ socket.
--
-- Valid peers: __dealer__, __requester__, __router__
newtype Router
  = Router (MVar Zmq_socket)
  deriving stock (Eq)
  deriving (Socket) via (ThreadSafeSocket)

instance CanSend Router

instance CanReceive Router

-- | Open a __router__.
open :: IO (Either Error Router)
open = do
  Socket.openThreadSafeSocket ZMQ_ROUTER >>= \case
    Left err -> pure (Left err)
    Right socketVar -> do
      socket <- readMVar socketVar
      Socket.setOption socket ZMQ_ROUTER_MANDATORY 1 <&> \case
        Left err -> Left err
        Right () -> Right (Router socketVar)

-- | Bind a __router__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Router -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __router__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Router -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __router__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Router -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __router__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Router -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __routed message__ on a __router__ to a peer.
--
-- If the peer no longer exists, returns @EHOSTUNREACH@.
send :: Router -> ByteString -> ByteString -> IO (Either Error ())
send socket0 identity message =
  withSocket socket0 \socket ->
    Socket.send2 socket identity message

-- | Send a __routed multiframe message__ on a __router__ to a peer.
--
-- If the peer no longer exists, returns @EHOSTUNREACH@.
sends :: Router -> ByteString -> List.NonEmpty ByteString -> IO (Either Error ())
sends socket0 identity message =
  withSocket socket0 \socket ->
    Socket.sends socket (List.NonEmpty.cons identity message)

-- | Receive a __routed message__ on an __router__ from any peer (fair-queued).
receive :: Router -> IO (Either Error (ByteString, ByteString))
receive socket0 =
  withSocket socket0 \socket ->
    Socket.receives socket <&> \case
      Left err -> Left err
      Right (identity :| message0) ->
        Right
          ( identity,
            case message0 of
              [] -> ByteString.empty
              message : _ -> message
          )

-- | Receive a __routed multiframe message__ on an __router__ from any peer (fair-queued).
receives :: Router -> IO (Either Error (ByteString, List.NonEmpty ByteString))
receives socket0 =
  withSocket socket0 \socket ->
    Socket.receives socket <&> \case
      Left err -> Left err
      Right (identity :| message0) ->
        Right
          ( identity,
            case message0 of
              [] -> ByteString.empty :| []
              frame : frames -> frame :| frames
          )

-- | /Alias/: 'Zmq.canSend'
canSend :: Router -> a -> Event a
canSend =
  Socket.canSend

-- | /Alias/: 'Zmq.canReceive'
canReceive :: Router -> a -> Event a
canReceive =
  Socket.canReceive
