module Zmq.API.Subscribe
  ( subscribe
  ) where

import Zmq.API.SetSockOpt
import Zmq.Error
import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-setsockopt#toc56>
subscribe
  :: Ptr FFI.Socket
  -> ByteString
  -> IO ()
subscribe socket prefix =
  fix \again ->
    setByteStringSockOpt socket FFI.zMQ_SUBSCRIBE prefix >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= \case
          EINTR_ ->
            again

          errno ->
            exception "zmq_setsockopt" errno
