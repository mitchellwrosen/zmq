module Zmq.API.Unbind
  ( unbind
  ) where

import qualified Zmqhs

import Zmq.Endpoint
import Zmq.Error
import Zmq.Exception
import Zmq.Internal (renderEndpoint)


-- | <http://api.zeromq.org/4-3:zmq-unbind>
unbind
  :: Zmqhs.Socket
  -> Endpoint transport
  -> IO ()
unbind socket endpoint =
  Zmqhs.unbind socket ( renderEndpoint endpoint ) >>= \case
    -- The endpoint supplied was not previously bound.
    Left ENOENT -> pure ()
    Left errno -> exception "zmq_unbind" errno
    Right () -> pure ()
