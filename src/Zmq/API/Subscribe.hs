module Zmq.API.Subscribe
  ( subscribe
  , subscribeIO'
  ) where

import Zmq.API.SetSockOpt
import Zmq.Error
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-setsockopt#toc56>
subscribe
  :: MonadIO m
  => Socket 'Sub
  -> ByteString
  -> m ()
subscribe sock prefix =
  liftIO ( subscribeIO sock prefix )

subscribeIO
  :: Socket 'Sub
  -> ByteString
  -> IO ()
subscribeIO sock prefix =
  fix \again ->
    setByteStringSockOpt sock FFI.zMQ_SUBSCRIBE prefix >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= \case
          EINTR_ ->
            again

          ETERM_ ->
            errInvalidContext

          -- EINVAL: type system should prevent it ->

          errno ->
            bugUnexpectedErrno "zmq_setsockopt" errno

subscribeIO'
  :: ForeignPtr FFI.Socket
  -> ByteString
  -> IO ()
subscribeIO' socket prefix =
  fix \again ->
    setByteStringSockOpt' socket FFI.zMQ_SUBSCRIBE prefix >>= \case
      0 ->
        pure ()

      _ ->
        FFI.zmq_errno >>= \case
          EINTR_ ->
            again

          ETERM_ ->
            errInvalidContext

          errno ->
            bugUnexpectedErrno "zmq_setsockopt" errno
