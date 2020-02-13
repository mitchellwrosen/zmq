module Zmq.API.GetSockOpt
  ( getSocketEvents
  , getSocketFd
  ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (peek, poke, sizeOf)
import System.Posix.Types (Fd(..))

import qualified Libzmq
import qualified Zmq.FFI as FFI

import qualified Zmqhs

import Zmq.Error
import Zmq.Exception
import Zmq.Prelude


retrying
  :: ( Zmqhs.Socket -> IO ( Either CInt a ) )
  -> Zmqhs.Socket
  -> IO a
retrying action socket =
  fix \again ->
    action socket >>= \case
      Left EINTR -> again
      Left errno -> exception "zmq_getsockopt" errno
      Right n -> pure n

getSocketEvents
  :: Zmqhs.Socket
  -> IO CInt
getSocketEvents =
  retrying Zmqhs.getSocketEvents

getSocketFd
  :: Zmqhs.Socket
  -> IO Fd
getSocketFd =
  fmap Fd . retrying Zmqhs.getSocketFd
