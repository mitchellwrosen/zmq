module Zmq.API.Connect
  ( connect
  ) where

import qualified Libzmq
import qualified Zmq.FFI as FFI

import Zmq.Endpoint
import Zmq.Exception
import Zmq.Prelude


-- | <http://api.zeromq.org/4-3:zmq-connect>
connect
  :: Ptr Libzmq.Socket
  -> Endpoint transport
  -> IO ()
connect socket endpoint =
  withEndpoint endpoint \c_endpoint ->
    Libzmq.connect socket c_endpoint >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= exception "zmq_connect"
