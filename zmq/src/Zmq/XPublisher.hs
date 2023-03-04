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

import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as List.NonEmpty
import Libzmq
import UnliftIO
import Zmq.Endpoint
import Zmq.Error (Error)
import Zmq.Internal.Context qualified as Zmq.Internal.Socket
import Zmq.SubscriptionMessage

newtype XPublisher
  = XPublisher (MVar Zmq_socket)
  deriving stock (Eq)

open :: IO (Either Error XPublisher)
open =
  coerce (Zmq.Internal.Socket.openThreadSafeSocket ZMQ_XPUB)

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

send :: XPublisher -> ByteString -> [ByteString] -> IO (Either Error ())
send (XPublisher socketVar) topic message =
  withMVar socketVar \socket -> Zmq.Internal.Socket.send socket (topic List.NonEmpty.:| message)

receive :: XPublisher -> IO (Either Error SubscriptionMessage)
receive (XPublisher socketVar) =
  withMVar socketVar \socket -> do
    let loop = do
          Zmq.Internal.Socket.receive socket >>= \case
            Left err -> pure (Left err)
            Right (UnsubscribeMessage prefix) -> pure (Right (Unsubscribe prefix))
            Right (SubscribeMessage prefix) -> pure (Right (Subscribe prefix))
            _ -> loop
    loop
