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
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty)
import Libzmq qualified
import UnliftIO
import Zmq.Context
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Socket qualified
import Zmq.SubscriptionMessage

newtype XPublisher
  = XPublisher (MVar Libzmq.Zmq_socket_t)
  deriving stock (Eq)

with :: forall a. Context -> (XPublisher -> IO (Either Error a)) -> IO (Either Error a)
with =
  coerce @(Context -> (MVar Libzmq.Zmq_socket_t -> IO (Either Error a)) -> IO (Either Error a)) Zmq.Internal.Socket.with

bind :: XPublisher -> Endpoint transport -> IO (Either Error ())
bind =
  coerce Zmq.Internal.Socket.bind

unbind :: XPublisher -> Endpoint transport -> IO (Either Error ())
unbind =
  coerce Zmq.Internal.Socket.unbind

connect :: XPublisher -> Endpoint transport -> IO (Either Error ())
connect =
  coerce Zmq.Internal.Socket.connect

disconnect :: XPublisher -> Endpoint transport -> IO (Either Error ())
disconnect =
  coerce Zmq.Internal.Socket.disconnect

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
