module Zmq.API.Bind
  ( bind
  ) where

import qualified Libzmq
import qualified Zmq.FFI as FFI

import Zmq.Endpoint
import Zmq.Exception
import Zmq.Prelude


-- | <http://api.zeromq.org/4-3:zmq-bind>
bind
  :: Ptr Libzmq.Socket
  -> Endpoint transport
  -> IO ()
bind socket endpoint =
  withEndpoint endpoint \c_endpoint ->
    Libzmq.bind socket c_endpoint >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= exception "zmq_bind"
