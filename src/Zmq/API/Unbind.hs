module Zmq.API.Unbind
  ( unbind
  ) where

import Zmq.Endpoint
import Zmq.Error
import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-unbind>
unbind
  :: Ptr FFI.Socket
  -> Endpoint transport
  -> IO ()
unbind socket endpoint =
  withEndpoint endpoint \c_endpoint ->
    FFI.zmq_unbind socket c_endpoint >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= \case
          -- The endpoint supplied was not previously bound.
          ENOENT_ ->
            pure ()

          errno ->
            exception "zmq_unbind" errno
