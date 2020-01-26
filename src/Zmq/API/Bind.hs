module Zmq.API.Bind
  ( bind
  , BindError
  ) where


import Zmq.Error
import Zmq.Internal
import Zmq.Function
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


type BindError
  = Error 'Function'Bind

-- | <http://api.zeromq.org/4-3:zmq-bind>
bind
  :: ( CompatibleTransport typ transport
     , MonadIO m
     )
  => Socket typ
  -> Endpoint transport
  -> m ( Either BindError () )
bind sock endpoint =
  liftIO ( bindIO sock endpoint )

bindIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either BindError () )
bindIO sock endpoint =
  withForeignPtr ( unSocket sock ) \ptr ->
    withCString ( endpointToString endpoint ) \c_endpoint ->
      FFI.zmq_bind ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          FFI.zmq_errno <&> \case
            EADDRINUSE_    -> Left EADDRINUSE
            EADDRNOTAVAIL_ -> Left EADDRNOTAVAIL
            EINVAL_        -> Left EINVAL
            EMTHREAD_      -> Left EMTHREAD
            ENODEV_        -> Left ENODEV

            ENOTSOCK_      -> Right ()
            ETERM_         -> Right ()

            -- ENOCOMPATPROTO: CompatibleTransport should prevent it
            -- EPROTONOSUPPORT: CPP should prevent it

            n -> errUnexpectedErrno "zmq_bind" n

