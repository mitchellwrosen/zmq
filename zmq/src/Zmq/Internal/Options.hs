module Zmq.Internal.Options
  ( Options (..),
    defaultOptions,
  )
where

import Foreign.C.Types (CInt)
import Libzmq.Bindings qualified
import Numeric.Natural (Natural)

data Options = Options
  { -- | The number of background IO threads that ØMQ uses.
    --
    -- As a rule of thumb, each thread can handle 1Gb/sec in or out. If your program performs no external socket IO, you
    -- can set this value to 0.
    --
    -- /Default/: 1
    ioThreads :: !Natural,
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
