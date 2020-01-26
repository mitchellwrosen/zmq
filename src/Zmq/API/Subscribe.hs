module Zmq.API.Subscribe
  ( subscribe
  ) where

import Zmq.API.SetSockOpt
import Zmq.Error
import Zmq.FFI
import Zmq.Prelude
import Zmq.Socket


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
    setByteStringSockOpt sock zMQ_SUBSCRIBE prefix >>= \case
      0 ->
        pure ()

      _ ->
        zmq_errno >>= \case
          EINTR_    -> again
          ENOTSOCK_ -> pure ()
          ETERM_    -> pure ()

          -- EINVAL: type system should prevent it ->

          n -> errUnexpectedErrno "zmq_setsockopt" n
