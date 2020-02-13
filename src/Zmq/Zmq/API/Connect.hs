module Zmq.API.Connect
  ( connect
  ) where

import Zmq.Endpoint
import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-connect>
connect
  :: Ptr FFI.Socket
  -> Endpoint transport
  -> IO ()
connect socket endpoint =
  withEndpoint endpoint \c_endpoint ->
    FFI.zmq_connect socket c_endpoint >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= exception "zmq_connect"
