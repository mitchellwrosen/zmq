module Zmqhs.SocketType
  ( SocketType(..)
  , pUB
  , sUB
  , xPUB
  , xSUB
  ) where

import Foreign.C (CInt)

import Libzmq.Constants


newtype SocketType
  = SocketType
  { unSocketType :: CInt }

pUB, sUB, xPUB, xSUB :: SocketType
pUB = SocketType zMQ_PUB
sUB = SocketType zMQ_SUB
xPUB = SocketType zMQ_XPUB
xSUB = SocketType zMQ_XSUB
