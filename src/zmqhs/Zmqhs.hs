module Zmqhs
  ( Context(..)
  , newContext
  , terminateContext
  , setContextOption
  , ContextOption(..)
  , ioThreads
  , maxSockets

  , Socket(..)
  , open
  , close
  , with
  , bind
  , unbind
  , connect
  , disconnect
  , getSocketEvents
  , getSocketFd
  , setSocketSubscribe
  , setSocketUnsubscribe

  , SocketType(..)

  , Endpoint(..)

  , Error(..)
  , throwError -- TODO don't export this
  , pattern EADDRINUSE
  , pattern EADDRNOTAVAIL
  , pattern EAGAIN
  , pattern EFAULT
  , pattern EHOSTUNREACH
  , pattern EINTR
  , pattern EINVAL
  , pattern EMFILE
  , pattern EMTHREAD
  , pattern ENODEV
  , pattern ENOENT
  , pattern ENOTSOCK
  , pattern ETERM
  ) where

import Zmqhs.Context
import Zmqhs.Endpoint
import Zmqhs.Error
import Zmqhs.Internal.Error (throwError)
import Zmqhs.Socket
import Zmqhs.SocketType
