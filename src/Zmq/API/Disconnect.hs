module Zmq.API.Disconnect
  ( disconnect
  , disconnectIO'
  ) where

import Zmq.Endpoint
import Zmq.Error
import Zmq.Internal
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-disconnect>
disconnect
  :: ( CompatibleTransport typ transport
     , MonadIO m
     )
  => Socket typ
  -> Endpoint transport
  -> m ()
disconnect socket endpoint =
  liftIO ( disconnectIO socket endpoint )

disconnectIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ()
disconnectIO socket endpoint =
  withSocket socket \socket_ptr ->
    withEndpoint endpoint \endpoint_ptr ->
      FFI.zmq_disconnect socket_ptr endpoint_ptr >>= \case
        0 ->
          pure ()

        _ ->
          FFI.zmq_errno >>= \case
            EINVAL_ -> pure () -- The endpoint supplied is invalid.
            ENOENT_ -> pure () -- The provided endpoint is not connected.


            ETERM_ ->
              errInvalidContext

            errno ->
              bugUnexpectedErrno "zmq_disconnect" errno

disconnectIO'
  :: ForeignPtr FFI.Socket
  -> Endpoint transport
  -> IO ()
disconnectIO' socket endpoint =
  withForeignPtr socket \socket_ptr ->
    withEndpoint endpoint \c_endpoint ->
      FFI.zmq_disconnect socket_ptr c_endpoint >>= \case
        0 ->
          pure ()

        _ ->
          FFI.zmq_errno >>= \case
            EINVAL_ -> pure () -- The endpoint supplied is invalid.
            ENOENT_ -> pure () -- The provided endpoint is not connected.

            ETERM_ ->
              errInvalidContext

            errno ->
              bugUnexpectedErrno "zmq_disconnect" errno
