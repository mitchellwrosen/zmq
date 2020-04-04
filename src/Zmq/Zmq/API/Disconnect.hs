module Zmq.API.Disconnect
  ( disconnect
  ) where

import qualified Zmqhs

import Zmq.Endpoint
import Zmq.Internal (renderEndpoint)


-- | <http://api.zeromq.org/4-3:zmq-disconnect>
disconnect
  :: Zmqhs.Socket
  -> Endpoint transport
  -> IO ()
disconnect socket endpoint =
  Zmqhs.disconnect socket ( renderEndpoint endpoint ) >>= \case
    -- The provided endpoint is not connected.
    Left Zmqhs.ENOENT -> pure ()
    Left errno -> Zmqhs.throwError "zmq_disconnect" errno
    Right () -> pure ()
