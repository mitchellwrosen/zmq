module Zmq.API.Socket
  ( socket
  ) where

import qualified Zmqhs

import Zmq.Context (Context(..))
import Zmq.Exception


-- | <http://api.zeromq.org/4-3:zmq-socket>
socket
  :: Context
  -> Zmqhs.SocketType
  -> IO Zmqhs.Socket
socket context socketType =
  Zmqhs.socket ( unContext context ) socketType >>= \case
    Left errno -> exception "zmq_socket" errno
    Right sock -> pure sock
