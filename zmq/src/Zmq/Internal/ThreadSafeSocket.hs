{-# LANGUAGE MagicHash #-}

module Zmq.Internal.ThreadSafeSocket
  ( ThreadSafeSocket,
    raw,
    name,
    with,
    open,
  )
where

import Control.Concurrent.MVar
import Data.Text (Text)
import GHC.MVar (MVar (..))
import Libzmq
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket qualified as Socket

data ThreadSafeSocket = ThreadSafeSocket
  { lock :: !(MVar ()),
    socket :: !Zmq_socket,
    _name :: !Text
  }
  deriving stock (Eq)

raw :: ThreadSafeSocket -> Zmq_socket
raw ThreadSafeSocket {socket} =
  socket

name :: ThreadSafeSocket -> Text
name ThreadSafeSocket {_name} =
  _name

with :: ThreadSafeSocket -> IO a -> IO a
with ThreadSafeSocket {lock, socket} action =
  withMVar lock \_ ->
    Socket.keepingSocketAlive socket action

-- Throws ok errors
open :: Zmq_socket_type -> Options socket -> IO ThreadSafeSocket
open socketType options = do
  socket <-
    Socket.openWith
      ( \socket -> do
          lock@(MVar canary#) <- newMVar ()
          pure $
            Socket.ThingAndCanary
              ThreadSafeSocket
                { lock,
                  socket,
                  _name = Options.optionsName options
                }
              canary#
      )
      socketType
  -- It's important that we call setSocketOptions after openWith returns, because setSocketOptions might throw an
  -- exception, in which case we want to properly finalize the socket
  Options.setSocketOptions (raw socket) options
  pure socket
