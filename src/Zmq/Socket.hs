{-# LANGUAGE UndecidableInstances #-}

module Zmq.Socket
  ( waitUntilCanRecv
  , waitUntilCanSend
  ) where

import Data.Bits (testBit)

import Zmq.API.GetSockOpt (getSocketEventState, getSocketFd)
import Zmq.Error
import Zmq.Prelude
import qualified Zmq.FFI as FFI


waitUntilCanRecv
  :: Ptr FFI.Socket
  -> Bool
  -> IO ()
waitUntilCanRecv =
  waitUntilCan FFI.zMQ_POLLIN

waitUntilCanSend
  :: Ptr FFI.Socket
  -> Bool
  -> IO ()
waitUntilCanSend =
  waitUntilCan FFI.zMQ_POLLOUT

waitUntilCan
  :: CInt
  -> Ptr FFI.Socket
  -> Bool
  -> IO ()
waitUntilCan events socket threadSafe =
  if threadSafe then
    bugIO "handling EAGAIN on thread-safe sockets is not implemented"
  else do
    fd <- getSocketFd socket

    fix \again -> do
      threadWaitRead fd -- "read" is not a typo
      state <- getSocketEventState socket
      -- http://api.zeromq.org/4-3:zmq-getsockopt
      --
      -- The combination of a file descriptor returned by
      -- the ZMQ_FD option being ready for reading but no
      -- actual events returned by a subsequent retrieval of
      -- the ZMQ_EVENTS option is valid; applications should
      -- simply ignore this case and restart their polling
      -- operation/event loop.
      unless ( testBit state ( fromIntegral events ) ) again
