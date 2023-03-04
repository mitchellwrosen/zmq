module Zmq.XPublisher
  ( XPublisher,
    with,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    receive,
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Libzmq qualified
import UnliftIO
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Socket qualified
import Zmq.SubscriptionMessage

newtype XPublisher
  = XPublisher (MVar Libzmq.Zmq_socket_t)
  deriving stock (Eq)

with :: (XPublisher -> IO (Either Error a)) -> IO (Either Error a)
with action =
  Zmq.Internal.Socket.with \socket -> do
    socketVar <- newMVar socket
    action (XPublisher socketVar)

bind :: XPublisher -> Endpoint transport -> IO (Either Error ())
bind (XPublisher socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.bind socket endpoint

unbind :: XPublisher -> Endpoint transport -> IO (Either Error ())
unbind (XPublisher socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.unbind socket endpoint

connect :: XPublisher -> Endpoint transport -> IO (Either Error ())
connect (XPublisher socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.connect socket endpoint

disconnect :: XPublisher -> Endpoint transport -> IO (Either Error ())
disconnect (XPublisher socketVar) endpoint =
  withMVar socketVar \socket -> Zmq.Internal.Socket.disconnect socket endpoint

send :: XPublisher -> NonEmpty ByteString -> IO (Either Error ())
send (XPublisher socketVar) message =
  withMVar socketVar \socket -> Zmq.Internal.Socket.send socket message

receive :: XPublisher -> IO (Either Error SubscriptionMessage)
receive (XPublisher socketVar) =
  withMVar socketVar \socket -> do
    let loop = do
          -- Zmqhs.receive socket >>= \case
          Zmq.Internal.Socket.receive socket >>= \case
            Left err -> pure (Left err)
            Right (UnsubscribeMessage prefix) -> pure (Right (Unsubscribe prefix))
            Right (SubscribeMessage prefix) -> pure (Right (Subscribe prefix))
            _ -> loop
    loop
