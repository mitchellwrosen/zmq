module Zmq.Internal.Socket (Socket) where

import Data.Kind (Type)

class Socket (socket :: Type)
