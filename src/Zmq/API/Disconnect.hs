module Zmq.API.Disconnect
  ( disconnect
  , DisconnectError
  ) where

import Zmq.Endpoint
import Zmq.Error
import Zmq.Function
import Zmq.Internal
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


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
disconnect socket endpoint =
  liftIO ( disconnectIO socket endpoint )

disconnectIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either DisconnectError () )
disconnectIO socket endpoint =
  withSocket socket \ptr ->
    withEndpoint endpoint \c_endpoint ->
      FFI.zmq_disconnect ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          FFI.zmq_errno <&> \case
            EINVAL_   -> Left EINVAL

            ENOENT_   -> Right ()
            ETERM_    -> Right ()

            n -> bugUnexpectedErrno "zmq_disconnect" n

