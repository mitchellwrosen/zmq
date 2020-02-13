module Zmq.API.Disconnect
  ( disconnect
  ) where

import qualified Libzmq
import qualified Zmq.FFI as FFI

import Zmq.Endpoint
import Zmq.Error
import Zmq.Exception
import Zmq.Prelude


-- | <http://api.zeromq.org/4-3:zmq-disconnect>
disconnect
  :: Ptr Libzmq.Socket
  -> Endpoint transport
  -> IO ()
disconnect socket endpoint =
  withEndpoint endpoint \c_endpoint ->
    Libzmq.disconnect socket c_endpoint >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= \case
          -- The provided endpoint is not connected.
          ENOENT ->
            pure ()

          errno ->
            exception "zmq_disconnect" errno
