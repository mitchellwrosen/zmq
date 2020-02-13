module Zmq.API.Subscribe
  ( subscribe
  ) where

import qualified Zmqhs

import Zmq.Error
import Zmq.Exception
import Zmq.Prelude


-- | <http://api.zeromq.org/4-3:zmq-setsockopt#toc56>
subscribe
  :: Zmqhs.Socket
  -> ByteString
  -> IO ()
subscribe socket prefix =
  fix \again ->
    Zmqhs.setSocketSubscribe socket prefix >>= \case
      Left EINTR -> again
      Left errno -> exception "zmq_setsockopt" errno
      Right () -> pure ()
