module Zmqhs
  ( -- * Socket
    getSocketEvents,
    getSocketFd,
    setSocketSubscribe,
    setSocketUnsubscribe,
    send,
    receive,

    -- * Error
    Error (..),
    throwError, -- TODO don't export this
  )
where

import Zmqhs.Error
import Zmqhs.Internal.Error (throwError)
import Zmqhs.Socket
