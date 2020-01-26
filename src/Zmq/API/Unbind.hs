module Zmq.API.Unbind
  ( unbind
  , UnbindError
  ) where

import Zmq.Error
import Zmq.Function
import Zmq.Internal
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


type UnbindError
  = Error 'Function'Unbind

unbind
  :: ( CompatibleTransport typ transport
     , MonadIO m
     )
  => Socket typ
  -> Endpoint transport
  -> m ( Either UnbindError () )
unbind sock endpoint =
  liftIO ( unbindIO sock endpoint )

unbindIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either UnbindError () )
unbindIO sock endpoint =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( endpointToString endpoint ) \c_endpoint ->
      FFI.zmq_unbind ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          FFI.zmq_errno <&> \case
            EINVAL_   -> Right ()
            ENOENT_   -> Right ()
            ENOTSOCK_ -> Right ()
            ETERM_    -> Right ()

            n -> errUnexpectedErrno "zmq_unbind" n
