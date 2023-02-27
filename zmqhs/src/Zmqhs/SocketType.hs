module Zmqhs.SocketType
  ( SocketType (..),
  )
where

import GHC.Generics (Generic)

data SocketType
  = Pub
  | Sub
  | XPub
  | XSub
  deriving stock (Eq, Generic, Show)
