module Zmq.API.Unbind
  ( unbind
  ) where

import Zmq.Endpoint
import Zmq.Error
import Zmq.Internal
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-unbind>
unbind
  :: ( CompatibleTransport typ transport
     , MonadIO m
     )
  => Socket typ
  -> Endpoint transport
  -> m ()
unbind socket endpoint =
  liftIO ( unbindIO socket endpoint )

unbindIO
  :: CompatibleTransport typ transport
  => Socket typ
  -> Endpoint transport
  -> IO ()
unbindIO socket endpoint =
  withSocket socket \ptr ->
    withEndpoint endpoint \c_endpoint ->
      FFI.zmq_unbind ptr c_endpoint >>= \case
        0 ->
          pure ()

        _ ->
          FFI.zmq_errno >>= \case
            EINVAL_ -> pure ()
            ENOENT_ -> pure ()

            ETERM_ ->
              errInvalidContext

            errno ->
              bugUnexpectedErrno "zmq_unbind" errno
