module Zmq.API.Disconnect
  ( disconnect
  ) where

import Zmq.Endpoint
import Zmq.Error
import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-disconnect>
disconnect
  :: Ptr FFI.Socket
  -> Endpoint transport
  -> IO ()
disconnect socket endpoint =
  withEndpoint endpoint \c_endpoint ->
    FFI.zmq_disconnect socket c_endpoint >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= \case
          -- The provided endpoint is not connected.
          ENOENT ->
            pure ()

          errno ->
            exception "zmq_disconnect" errno
