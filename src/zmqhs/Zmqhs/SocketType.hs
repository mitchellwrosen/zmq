module Zmqhs.SocketType
  ( SocketType(..)
  , pub
  , sub
  , xpub
  , xsub
  ) where

import Foreign.C (CInt)

import Libzmq.Constants


newtype SocketType
  = SocketType CInt

pub, sub, xpub, xsub :: SocketType
pub = SocketType zMQ_PUB
sub = SocketType zMQ_SUB
xpub = SocketType zMQ_XPUB
xsub = SocketType zMQ_XSUB
