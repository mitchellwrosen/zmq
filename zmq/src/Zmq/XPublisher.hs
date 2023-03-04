module Zmq.XPublisher
  ( XPublisher,
    open,
    send,
    receive,
  )
where

import Control.Concurrent.MVar
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as List.NonEmpty
import Libzmq
import Zmq.Error (Error)
import Zmq.Internal.Socket (Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.SubscriptionMessage
  ( SubscriptionMessage (Subscribe, Unsubscribe),
    pattern SubscribeMessage,
    pattern UnsubscribeMessage,
  )

newtype XPublisher
  = XPublisher (MVar Zmq_socket)
  deriving stock (Eq)

instance Socket XPublisher where
  withSocket (XPublisher socketVar) =
    withMVar socketVar

open :: IO (Either Error XPublisher)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_XPUB)

send :: XPublisher -> ByteString -> [ByteString] -> IO (Either Error ())
send socket0 topic message =
  withSocket socket0 \socket ->
    Socket.send socket (topic List.NonEmpty.:| message)

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
