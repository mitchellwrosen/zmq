module Zmq.API.Connect
  ( connect
  , ConnectError
  ) where

import Zmq.Error
import Zmq.FFI
import Zmq.Function
import Zmq.Internal
import Zmq.Prelude
import Zmq.Socket


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
connect sock endpoint =
  liftIO ( connectIO sock endpoint )

connectIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either ConnectError () )
connectIO sock endpoint =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( endpointToString endpoint ) \c_endpoint ->
      zmq_connect ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          zmq_errno <&> \case
            EINVAL_   -> Left EINVAL
            EMTHREAD_ -> Left EMTHREAD

            ENOTSOCK_ -> Right ()
            ETERM_    -> Right ()

            -- ENOCOMPATPROTO: CompatibleTransport should prevent it
            -- EPROTONOSUPPORT: CPP should prevent it

            n -> errUnexpectedErrno "zmq_connect" n

