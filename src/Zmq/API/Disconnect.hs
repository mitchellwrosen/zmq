module Zmq.API.Disconnect
  ( disconnect
  , DisconnectError
  ) where

import Zmq.Error
import Zmq.FFI
import Zmq.Function
import Zmq.Internal
import Zmq.Prelude
import Zmq.Socket


type DisconnectError
  = Error 'Function'Disconnect

-- | <http://api.zeromq.org/4-3:zmq-disconnect>
disconnect
  :: ( CompatibleTransport typ transport
     , MonadIO m
     )
  => Socket typ
  -> Endpoint transport
  -> m ( Either DisconnectError () )
disconnect sock endpoint =
  liftIO ( disconnectIO sock endpoint )

disconnectIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either DisconnectError () )
disconnectIO sock endpoint =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( endpointToString endpoint ) \c_endpoint ->
      zmq_disconnect ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          zmq_errno <&> \case
            EINVAL_   -> Left EINVAL

            ENOENT_   -> Right ()
            ENOTSOCK_ -> Right ()
            ETERM_    -> Right ()

            n -> errUnexpectedErrno "zmq_disconnect" n

