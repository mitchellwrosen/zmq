{-# LANGUAGE MagicHash #-}

module Zmq.Internal.ThreadUnsafeSocket
  ( ThreadUnsafeSocket,
    raw,
    name,
    with,
    open,
  )
where

import Data.IORef (IORef, newIORef)
import Data.Text (Text)
import GHC.IORef (IORef (..))
import GHC.STRef (STRef (..))
import Libzmq
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket qualified as Socket

data ThreadUnsafeSocket = ThreadUnsafeSocket
  { socket :: !Zmq_socket,
    _name :: !Text,
    canary :: !(IORef ())
  }

instance Eq ThreadUnsafeSocket where
  ThreadUnsafeSocket s0 _ _ == ThreadUnsafeSocket s1 _ _ =
    s0 == s1

instance Ord ThreadUnsafeSocket where
  compare (ThreadUnsafeSocket s0 _ _) (ThreadUnsafeSocket s1 _ _) =
    compare s0 s1

raw :: ThreadUnsafeSocket -> Zmq_socket
raw ThreadUnsafeSocket {socket} =
  socket

name :: ThreadUnsafeSocket -> Text
name ThreadUnsafeSocket {_name} =
  _name

with :: ThreadUnsafeSocket -> IO a -> IO a
with ThreadUnsafeSocket {socket} =
  Socket.keepingSocketAlive socket

-- Throws ok errors
open :: Zmq_socket_type -> Options socket -> IO ThreadUnsafeSocket
open socketType options = do
  socket <-
    Socket.openWith
      ( \socket -> do
          canary@(IORef (STRef canary#)) <- newIORef ()
          pure $
            Socket.ThingAndCanary
              ThreadUnsafeSocket
                { socket,
                  _name = Options.optionsName options,
                  canary
                }
              canary#
      )
      socketType
  -- It's important that we call setSocketOptions after openWith returns, because setSocketOptions might throw an
  -- exception, in which case we want to properly finalize the socket
  Options.setSocketOptions (raw socket) options
  pure socket
