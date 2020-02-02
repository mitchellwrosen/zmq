module Zmq.API.Connect
  ( connect
  , ConnectError
  ) where

import Zmq.Endpoint
import Zmq.Exception (exception)
import Zmq.Error
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- |
-- @
-- data ConnectError
--   = 'EINVAL'
--   | 'EMTHREAD'
-- @
type ConnectError
  = Error "connect"

-- | <http://api.zeromq.org/4-3:zmq-connect>
connect
  :: ForeignPtr FFI.Socket
  -> Endpoint transport
  -> IO ( Either ConnectError () )
connect socket endpoint =
  withForeignPtr socket \socket_ptr ->
    withEndpoint endpoint \c_endpoint ->
      FFI.zmq_connect socket_ptr c_endpoint >>= \case
        0 ->
          pure ( Right () )

        _ ->
          FFI.zmq_errno >>= \case
            EINVAL_   -> pure ( Left EINVAL )
            EMTHREAD_ -> pure ( Left EMTHREAD )

            errno ->
              if errno == ETERM_ then
                exception "zmq_connect" errno
              else
                bugUnexpectedErrno "zmq_connect" errno
