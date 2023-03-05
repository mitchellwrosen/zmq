module Zmq.Dealer
  ( Dealer,
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
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error (..))
import Zmq.Internal.Socket (CanReceive, CanSend, Event, Socket (withSocket), ThreadSafeSocket)
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __dealer__ socket.
--
-- Valid peers: __dealer__, __replier__, __router__
newtype Dealer
  = Dealer (MVar Zmq_socket)
  deriving stock (Eq)
  deriving (Socket) via (ThreadSafeSocket)

instance CanSend Dealer

instance CanReceive Dealer

-- | Open an __dealer__.
open :: IO (Either Error Dealer)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_DEALER)

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
send socket0 message = do
  let loop =
        withSocket socket0 \socket ->
          Socket.send socket message >>= \case
            Left Error {errno = EAGAIN} -> do
              Socket.blockUntilCanSend socket >>= \case
                Left err -> pure (Left err)
                Right () -> loop
            Left err -> pure (Left err)
            Right () -> pure (Right ())
  loop

-- | Send a __multiframe message__ on a __dealer__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
sends :: Dealer -> List.NonEmpty ByteString -> IO (Either Error ())
sends socket0 message = do
  let loop =
        withSocket socket0 \socket ->
          Socket.sends socket message >>= \case
            Left Error {errno = EAGAIN} -> do
              Socket.blockUntilCanSend socket >>= \case
                Left err -> pure (Left err)
                Right () -> loop
            Left err -> pure (Left err)
            Right () -> pure (Right ())
  loop

-- | Receive a __message__ on an __dealer__ from any peer (fair-queued).
receive :: Dealer -> IO (Either Error ByteString)
receive socket =
  withSocket socket Socket.receive

-- | Receive a __multiframe message__ on an __dealer__ from any peer (fair-queued).
receives :: Dealer -> IO (Either Error (List.NonEmpty ByteString))
receives socket =
  withSocket socket Socket.receives

-- | /Alias/: 'Zmq.canSend'
canSend :: Dealer -> a -> Event a
canSend =
  Socket.canSend

-- | /Alias/: 'Zmq.canReceive'
canReceive :: Dealer -> a -> Event a
canReceive =
  Socket.canReceive
