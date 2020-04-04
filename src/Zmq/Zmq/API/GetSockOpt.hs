module Zmq.API.GetSockOpt
  ( getSocketEvents
  , getSocketFd
  ) where

import System.Posix.Types (Fd(..))

import qualified Zmqhs

import Zmq.Prelude


retrying
  :: ( Zmqhs.Socket -> IO ( Either CInt a ) )
  -> Zmqhs.Socket
  -> IO a
retrying action socket =
  fix \again ->
    action socket >>= \case
      Left Zmqhs.EINTR -> again
      Left errno -> Zmqhs.throwError "zmq_getsockopt" errno
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
