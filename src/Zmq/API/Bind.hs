module Zmq.API.Bind
  ( bind
  , bindIO
  , bindIO'
  , BindError
  ) where


import Zmq.Endpoint
import Zmq.Error
import Zmq.Internal
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


-- |
-- @
-- data BindError
--   = 'EADDRINUSE'
--   | 'EADDRNOTAVAIL'
--   | 'EINVAL'
--   | 'EMTHREAD'
--   | 'ENODEV'
-- @
type BindError
  = Error "bind"

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
          FFI.zmq_errno >>= \case
            EADDRINUSE_    -> pure ( Left EADDRINUSE )
            EADDRNOTAVAIL_ -> pure ( Left EADDRNOTAVAIL )
            EINVAL_        -> pure ( Left EINVAL )
            EMTHREAD_      -> pure ( Left EMTHREAD )
            ENODEV_        -> pure ( Left ENODEV )

            ETERM_ ->
              errInvalidContext

            -- ENOCOMPATPROTO: CompatibleTransport should prevent it
            -- EPROTONOSUPPORT: CPP should prevent it

            errno ->
              bugUnexpectedErrno "zmq_bind" errno

bindIO'
  :: ForeignPtr FFI.Socket
  -> Endpoint transport
  -> IO ( Either BindError () )
bindIO' socket endpoint =
  withForeignPtr socket \socket_ptr ->
    withEndpoint endpoint \c_endpoint ->
      FFI.zmq_bind socket_ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          FFI.zmq_errno >>= \case
            EADDRINUSE_    -> pure ( Left EADDRINUSE )
            EADDRNOTAVAIL_ -> pure ( Left EADDRNOTAVAIL )
            EINVAL_        -> pure ( Left EINVAL )
            EMTHREAD_      -> pure ( Left EMTHREAD )
            ENODEV_        -> pure ( Left ENODEV )

            ETERM_ ->
              errInvalidContext

            errno ->
              bugUnexpectedErrno "zmq_bind" errno
