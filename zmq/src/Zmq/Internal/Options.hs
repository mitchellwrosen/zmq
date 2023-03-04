module Zmq.Internal.Options
  ( Options (..),
    defaultOptions,
  )
where

import Foreign.C.Types (CInt)
import Libzmq.Bindings qualified
import Numeric.Natural (Natural)

data Options = Options
  { ioThreads :: !Natural,
    maxMessageSize :: !Natural,
    maxSockets :: !Natural
  }

defaultOptions :: Options
defaultOptions =
  Options
    { ioThreads = fromIntegral Libzmq.Bindings._ZMQ_IO_THREADS_DFLT,
      maxMessageSize = fromIntegral @CInt maxBound,
      maxSockets = fromIntegral Libzmq.Bindings._ZMQ_MAX_SOCKETS_DFLT
    }
