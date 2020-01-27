module Zmq.API.Unbind
  ( unbindIO'
  ) where

import Zmq.Endpoint
import Zmq.Error
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-unbind>
unbindIO'
  :: ForeignPtr FFI.Socket
  -> Endpoint transport
  -> IO ()
unbindIO' socket endpoint =
  withForeignPtr socket \socket_ptr ->
    withEndpoint endpoint \c_endpoint ->
      FFI.zmq_unbind socket_ptr c_endpoint >>= \case
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
