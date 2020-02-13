module Zmq.Poller
  ( Poller(..)
  ) where

import qualified Zmq.FFI as FFI

newtype Poller
  = Poller { unPoller :: FFI.Poller }
