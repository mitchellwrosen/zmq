module Zmq.Internal.Socket (Socket) where

import GHC.TypeLits (Symbol)

type role Socket nominal

data Socket (a :: Symbol)
