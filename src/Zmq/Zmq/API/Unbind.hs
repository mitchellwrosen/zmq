module Zmq.API.Unbind
  ( unbind
  ) where

import qualified Zmq.FFI as FFI
import qualified Libzmq

import Zmq.Endpoint
import Zmq.Error
import Zmq.Exception
import Zmq.Prelude


-- | <http://api.zeromq.org/4-3:zmq-unbind>
unbind
  :: Ptr Libzmq.Socket
  -> Endpoint transport
  -> IO ()
unbind socket endpoint =
  withEndpoint endpoint \c_endpoint ->
    Libzmq.unbind socket c_endpoint >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= \case
          -- The endpoint supplied was not previously bound.
          ENOENT ->
            pure ()

          errno ->
            exception "zmq_unbind" errno
