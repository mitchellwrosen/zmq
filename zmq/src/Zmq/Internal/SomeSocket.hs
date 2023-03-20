module Zmq.Internal.SomeSocket
  ( SomeSocket (..),
  )
where

import Zmq.Internal.Socket (Socket (..))

data SomeSocket
  = forall a. SomeSocket (Socket a)

instance Eq SomeSocket where
  SomeSocket x == SomeSocket y = zsocket x == zsocket y
  SomeSocket x /= SomeSocket y = zsocket x /= zsocket y

instance Ord SomeSocket where
  compare (SomeSocket x) (SomeSocket y) = compare (zsocket x) (zsocket y)
  SomeSocket x < SomeSocket y = zsocket x < zsocket y
  SomeSocket x <= SomeSocket y = zsocket x <= zsocket y
  SomeSocket x > SomeSocket y = zsocket x > zsocket y
  SomeSocket x >= SomeSocket y = zsocket x >= zsocket y
