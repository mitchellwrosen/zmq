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
import Zmq.Internal.Socket qualified as Socket
import Zmq.SubscriptionMessage
  ( SubscriptionMessage (Subscribe, Unsubscribe),
    pattern SubscribeMessage,
    pattern UnsubscribeMessage,
  )

newtype XPublisher
  = XPublisher (MVar Zmq_socket)
  deriving stock (Eq)

open :: IO (Either Error XPublisher)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_XPUB)

bind :: XPublisher -> Endpoint transport -> IO (Either Error ())
bind (XPublisher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.bind socket endpoint

unbind :: XPublisher -> Endpoint transport -> IO (Either Error ())
unbind (XPublisher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.unbind socket endpoint

connect :: XPublisher -> Endpoint transport -> IO (Either Error ())
connect (XPublisher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.connect socket endpoint

disconnect :: XPublisher -> Endpoint transport -> IO (Either Error ())
disconnect (XPublisher socketVar) endpoint =
  withMVar socketVar \socket -> Socket.disconnect socket endpoint

send :: XPublisher -> ByteString -> [ByteString] -> IO (Either Error ())
send (XPublisher socketVar) topic message =
  withMVar socketVar \socket -> Socket.send socket (topic List.NonEmpty.:| message)

receive :: XPublisher -> IO (Either Error SubscriptionMessage)
receive (XPublisher socketVar) =
  withMVar socketVar \socket -> do
    let loop = do
          Socket.receive socket >>= \case
            Left err -> pure (Left err)
            Right (UnsubscribeMessage prefix) -> pure (Right (Unsubscribe prefix))
            Right (SubscribeMessage prefix) -> pure (Right (Subscribe prefix))
            _ -> loop
    loop
