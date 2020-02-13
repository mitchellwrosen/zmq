module Zmq.API.Disconnect
  ( disconnect
  ) where

import qualified Zmqhs

import Zmq.Endpoint
import Zmq.Error
import Zmq.Exception
import Zmq.Internal (renderEndpoint)


-- | <http://api.zeromq.org/4-3:zmq-disconnect>
disconnect
  :: Zmqhs.Socket
  -> Endpoint transport
  -> IO ()
disconnect socket endpoint =
  Zmqhs.disconnect socket ( renderEndpoint endpoint ) >>= \case
    -- The provided endpoint is not connected.
    Left ENOENT -> pure ()
    Left errno -> exception "zmq_disconnect" errno
    Right () -> pure ()
