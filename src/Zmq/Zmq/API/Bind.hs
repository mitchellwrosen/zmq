module Zmq.API.Bind
  ( bind
  ) where

import qualified Zmqhs

import Zmq.Endpoint
import Zmq.Exception
import Zmq.Internal (renderEndpoint)


-- | <http://api.zeromq.org/4-3:zmq-bind>
bind
  :: Zmqhs.Socket
  -> Endpoint transport
  -> IO ()
bind socket endpoint =
  Zmqhs.bind socket ( renderEndpoint endpoint ) >>= \case
    Left errno -> exception "zmq_bind" errno
    Right () -> pure ()
