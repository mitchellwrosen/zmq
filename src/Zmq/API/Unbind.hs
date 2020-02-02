module Zmq.API.Unbind
  ( unbind
  ) where

import Zmq.Endpoint
import Zmq.Error
import Zmq.Exception
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-unbind>
unbind
  :: ForeignPtr FFI.Socket
  -> Endpoint transport
  -> IO ()
unbind socket endpoint =
  withForeignPtr socket \socket_ptr ->
    withEndpoint endpoint \c_endpoint ->
      FFI.zmq_unbind socket_ptr c_endpoint >>= \case
        0 ->
          pure ()

        _ ->
          FFI.zmq_errno >>= \case
            EINVAL_ -> pure ()
            ENOENT_ -> pure ()

            errno ->
              if errno == ETERM_ then
                exception "zmq_unbind" errno
              else
                unexpectedErrno "zmq_unbind" errno
