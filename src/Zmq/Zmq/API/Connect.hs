module Zmq.API.Connect
  ( connect
  ) where

import qualified Zmqhs

import Zmq.Endpoint
import Zmq.Exception
import Zmq.Internal (renderEndpoint)


-- | <http://api.zeromq.org/4-3:zmq-connect>
connect
  :: Zmqhs.Socket
  -> Endpoint transport
  -> IO ()
connect socket endpoint =
  Zmqhs.connect socket ( renderEndpoint endpoint ) >>= \case
    Left errno -> exception "zmq_connect" errno
    Right () -> pure ()
