module Zmq.API.Subscribe
  ( subscribe
  ) where

import qualified Libzmq
import qualified Zmq.FFI as FFI

import qualified Zmqhs

import Zmq.API.SetSockOpt
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
    setByteStringSockOpt ( Zmqhs.unSocket socket ) FFI.zMQ_SUBSCRIBE prefix >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= \case
          EINTR ->
            again

          errno ->
            exception "zmq_setsockopt" errno
