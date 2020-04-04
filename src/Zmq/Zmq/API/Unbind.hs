module Zmq.API.Unbind
  ( unbind
  ) where

import qualified Zmqhs

import Zmq.Endpoint
import Zmq.Internal (renderEndpoint)


-- | <http://api.zeromq.org/4-3:zmq-unbind>
unbind
  :: Zmqhs.Socket
  -> Endpoint transport
  -> IO ()
unbind socket endpoint =
  Zmqhs.unbind socket ( renderEndpoint endpoint ) >>= \case
    -- The endpoint supplied was not previously bound.
    Left Zmqhs.ENOENT -> pure ()
    Left errno -> Zmqhs.throwError "zmq_unbind" errno
    Right () -> pure ()
