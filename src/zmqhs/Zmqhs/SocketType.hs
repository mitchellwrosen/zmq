module Zmqhs.SocketType
  ( SocketType(..)
  , pub
  , sub
  , xpub
  , xsub
  ) where

import Foreign.C (CInt)

import qualified Libzmq


newtype SocketType
  = SocketType
  { unSocketType :: CInt }

pub, sub, xpub, xsub :: SocketType
pub = SocketType Libzmq.pub
sub = SocketType Libzmq.sub
xpub = SocketType Libzmq.xpub
xsub = SocketType Libzmq.xsub
