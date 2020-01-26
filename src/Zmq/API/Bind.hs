module Zmq.API.Bind
  ( bind
  , BindError
  ) where


import Zmq.Endpoint
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
bind socket endpoint =
  liftIO ( bindIO socket endpoint )

bindIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ( Either BindError () )
bindIO socket endpoint =
  withSocket socket \ptr ->
    withEndpoint endpoint \c_endpoint ->
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

            ETERM_         -> Right ()

            -- ENOCOMPATPROTO: CompatibleTransport should prevent it
            -- EPROTONOSUPPORT: CPP should prevent it

            n -> bugUnexpectedErrno "zmq_bind" n

