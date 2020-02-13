module Zmq.API.PollerNew where
{-
module Zmq.API.PollerNew
  ( pollerNew
  ) where

import Zmq.Poller
import Zmq.Prelude
import qualified Zmq.FFI as FFI


pollerNew :: MonadIO m => m Poller
pollerNew =
  liftIO ( coerce FFI.zmq_poller_new )
-}
