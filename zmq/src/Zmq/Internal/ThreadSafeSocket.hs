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

with :: ThreadSafeSocket -> (Zmq_socket -> IO a) -> IO a
with ThreadSafeSocket {lock, socket} action =
  withMVar lock \_ ->
    Socket.keepingSocketAlive socket (action socket)

-- Throws ok errors
open :: Zmq_socket_type -> Options socket -> IO ThreadSafeSocket
open socketType options =
  Socket.openWith
    ( \socket -> do
        lock@(MVar canary#) <- newMVar ()
        Options.setSocketOptions socket options
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

