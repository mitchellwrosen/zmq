{-# LANGUAGE UndecidableInstances #-}

module Zmq.Socket
  ( nonThreadsafeWaitUntilCanRecv
  , nonThreadsafeWaitUntilCanSend
  ) where

import Data.Bits ((.&.))

import qualified Zmq.FFI as FFI

import qualified Zmqhs

import Zmq.API.GetSockOpt (getSocketEvents, getSocketFd)
import Zmq.Prelude


nonThreadsafeWaitUntilCanRecv
  :: Zmqhs.Socket
  -> IO ()
nonThreadsafeWaitUntilCanRecv =
  nonThreadsafeWaitUntilCan FFI.zMQ_POLLIN

nonThreadsafeWaitUntilCanSend
  :: Zmqhs.Socket
  -> IO ()
nonThreadsafeWaitUntilCanSend =
  nonThreadsafeWaitUntilCan FFI.zMQ_POLLOUT

nonThreadsafeWaitUntilCan
  :: CInt
  -> Zmqhs.Socket
  -> IO ()
nonThreadsafeWaitUntilCan events socket = do
  fd <- getSocketFd socket

  fix \again -> do
    threadWaitRead fd -- "read" is not a typo
    state <- getSocketEvents socket
    -- http://api.zeromq.org/4-3:zmq-getsockopt
    --
    -- The combination of a file descriptor returned by
    -- the ZMQ_FD option being ready for reading but no
    -- actual events returned by a subsequent retrieval of
    -- the ZMQ_EVENTS option is valid; applications should
    -- simply ignore this case and restart their polling
    -- operation/event loop.
    unless ( state .&. events /= 0 ) again
