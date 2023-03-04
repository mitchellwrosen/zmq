module Zmq.XPublisher
  ( XPublisher,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    receive,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error)
import Zmq.Internal.Socket (Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.SubscriptionMessage
  ( SubscriptionMessage (Subscribe, Unsubscribe),
    pattern SubscribeMessage,
    pattern UnsubscribeMessage,
  )

-- | A thread-safe __xpublisher__ socket.
--
-- Valid peers: __subscriber__, __xsubscriber__
newtype XPublisher
  = XPublisher (MVar Zmq_socket)
  deriving stock (Eq)

instance Socket XPublisher where
  withSocket (XPublisher socketVar) =
    withMVar socketVar

-- | Open an __xpublisher__.
open :: IO (Either Error XPublisher)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_XPUB)

-- | Bind an __xpublisher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: XPublisher -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind an __xpublisher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: XPublisher -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect an __xpublisher__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: XPublisher -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect an __xpublisher__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: XPublisher -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on an __xpublisher__ to all peers.
--
-- If a peer has a full message queue, it will not receive the message.
send :: XPublisher -> ByteString -> [ByteString] -> IO (Either Error ())
send socket0 topic message =
  withSocket socket0 \socket ->
    Socket.send socket (topic List.NonEmpty.:| message)

-- | Receive a __subscription message__ on an __xpublisher__ from any peer.
receive :: XPublisher -> IO (Either Error SubscriptionMessage)
receive socket0 =
  withSocket socket0 \socket -> do
    let loop = do
          Socket.receive socket >>= \case
            Left err -> pure (Left err)
            Right (UnsubscribeMessage prefix) -> pure (Right (Unsubscribe prefix))
            Right (SubscribeMessage prefix) -> pure (Right (Subscribe prefix))
            _ -> loop
    loop
