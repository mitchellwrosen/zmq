module Zmq.API.Connect
  ( connect
  , ConnectError
  ) where

import Zmq.Endpoint
import Zmq.Error
import Zmq.Function
import Zmq.Internal
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


type ConnectError
  = Error 'Function'Connect

-- | <http://api.zeromq.org/4-3:zmq-connect>
connect
  :: ( CompatibleTransport typ transport
     , MonadIO m
     )
  => Socket typ
  -> Endpoint transport
  -> m ( Either ConnectError () )
connect socket endpoint =
  liftIO ( connectIO socket endpoint )

connectIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either ConnectError () )
connectIO socket endpoint =
  withSocket socket \ptr ->
    withEndpoint endpoint \c_endpoint ->
      FFI.zmq_connect ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          FFI.zmq_errno >>= \case
            EINVAL_   -> pure ( Left EINVAL )
            EMTHREAD_ -> pure ( Left EMTHREAD )

            ETERM_ ->
              errInvalidContext

            -- ENOCOMPATPROTO: CompatibleTransport should prevent it
            -- EPROTONOSUPPORT: CPP should prevent it

            errno ->
              bugUnexpectedErrno "zmq_connect" errno
