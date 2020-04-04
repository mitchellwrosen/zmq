module Zmq.API.Subscribe
  ( subscribe
  ) where

import qualified Zmqhs

import Zmq.Prelude


-- | <http://api.zeromq.org/4-3:zmq-setsockopt#toc56>
subscribe
  :: Zmqhs.Socket
  -> ByteString
  -> IO ()
subscribe socket prefix =
  fix \again ->
    Zmqhs.setSocketSubscribe socket prefix >>= \case
      Left Zmqhs.EINTR -> again
      Left errno -> Zmqhs.throwError "zmq_setsockopt" errno
      Right () -> pure ()
