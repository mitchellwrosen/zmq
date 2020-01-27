{-# LANGUAGE UndecidableInstances #-}

module Zmq.Socket
  ( Socket(..)
  , withSocket
  , waitUntilCanRecv
  , waitUntilCanSend
  , CanReceive
  , CanSend
  , SocketType(..)
  , IsSocketType(..)
  ) where

import Data.Bits (testBit)
import GHC.Conc (threadWaitRead)
import qualified GHC.TypeLits as TypeLits

import Zmq.API.GetSockOpt (getSocketEventState, getSocketFd)
import Zmq.Error
import Zmq.Internal
import Zmq.Prelude
import qualified Zmq.FFI as FFI


newtype Socket ( a :: SocketType )
  = Socket
  { unSocket :: ForeignPtr () }
  deriving stock ( Eq, Data, Ord, Show )

withSocket
  :: Socket typ
  -> ( Ptr () -> IO a )
  -> IO a
withSocket socket =
  withForeignPtr ( unSocket socket )

waitUntilCanRecv
  :: FFI.Socket
  -> Bool
  -> IO ()
waitUntilCanRecv =
  waitUntilCan FFI.zMQ_POLLIN

waitUntilCanSend
  :: FFI.Socket
  -> Bool
  -> IO ()
waitUntilCanSend =
  waitUntilCan FFI.zMQ_POLLOUT

waitUntilCan
  :: CInt
  -> FFI.Socket
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


class IsSocketType typ => CanReceive ( typ :: SocketType )

instance
  {-# OVERLAPPABLE #-}
  ( IsSocketType typ
  , TypeLits.TypeError
    ( 'TypeLits.Text "Cannot receive on a "
      'TypeLits.:<>:
      'TypeLits.ShowType typ
      'TypeLits.:<>:
      'TypeLits.Text " socket."
    )
  )
  => CanReceive typ

instance CanReceive 'Sub
-- instance CanReceive 'XPub
-- instance CanReceive 'XSub

class IsSocketType typ => CanSend ( typ :: SocketType )

instance
  {-# OVERLAPPABLE #-}
  ( IsSocketType typ
  , TypeLits.TypeError
    ( 'TypeLits.Text "Cannot send on a "
      'TypeLits.:<>:
      'TypeLits.ShowType typ
      'TypeLits.:<>:
      'TypeLits.Text " socket."
    )
  )
  => CanSend typ

instance CanSend 'Pub


class IsSocketType ( typ :: SocketType ) where
  socketType :: CInt
  isThreadSafe :: Bool

instance IsSocketType 'Pub where
  socketType :: CInt
  socketType =
    FFI.zMQ_PUB

  isThreadSafe :: Bool
  isThreadSafe =
    False

instance IsSocketType 'Sub where
  socketType :: CInt
  socketType =
    FFI.zMQ_SUB

  isThreadSafe :: Bool
  isThreadSafe =
    False
