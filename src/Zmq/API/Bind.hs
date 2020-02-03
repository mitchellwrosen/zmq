module Zmq.API.Bind
  ( bind
  ) where


import Zmq.Endpoint
import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-bind>
bind
  :: Ptr FFI.Socket
  -> Endpoint transport
  -> IO ()
bind socket endpoint =
  withEndpoint endpoint \c_endpoint ->
    FFI.zmq_bind socket c_endpoint >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= exception "zmq_bind"
