{-# LANGUAGE UndecidableInstances #-}

module Zmq.Socket
  ( Socket(..)
  , SocketType(..)
  , IsSocketType(..)
  ) where

import qualified GHC.TypeLits as TypeLits

import Zmq.Internal
import Zmq.Prelude
import qualified Zmq.FFI as FFI


newtype Socket ( a :: SocketType )
  = Socket
  { unSocket :: ForeignPtr () }


type family CanReceive ( typ :: SocketType ) :: Constraint where
  CanReceive 'Sub = ()
  -- CanReceive 'XPub = ()
  -- CanReceive 'XSub = ()

  CanReceive typ =
    TypeLits.TypeError
      ( 'TypeLits.Text "Cannot receive on a "
        'TypeLits.:<>:
        'TypeLits.ShowType typ
        'TypeLits.:<>:
        'TypeLits.Text " socket."
      )

type family CanSend ( typ :: SocketType ) :: Constraint where
  CanSend 'Pub = ()

  CanSend typ =
    TypeLits.TypeError
      ( 'TypeLits.Text "Cannot send on a "
        'TypeLits.:<>:
        'TypeLits.ShowType typ
        'TypeLits.:<>:
        'TypeLits.Text " socket."
      )


class IsSocketType ( a :: SocketType ) where
  socketType :: CInt

instance IsSocketType 'Pub where
  socketType :: CInt
  socketType =
    FFI.zMQ_PUB

instance IsSocketType 'Sub where
  socketType :: CInt
  socketType =
    FFI.zMQ_SUB
