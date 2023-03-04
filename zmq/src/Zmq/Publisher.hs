module Zmq.Publisher
  ( Publisher,
    open,
    send,
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

newtype Publisher
  = Publisher (MVar Zmq_socket)
  deriving stock (Eq)

instance Socket Publisher where
  withSocket (Publisher socketVar) =
    withMVar socketVar

open :: IO (Either Error Publisher)
open =
  coerce (Socket.openThreadSafeSocket ZMQ_PUB)

send :: Publisher -> ByteString -> [ByteString] -> IO (Either Error ())
send socket0 topic message =
  withSocket socket0 \socket ->
    Socket.send socket (topic List.NonEmpty.:| message)
