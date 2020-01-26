{-# LANGUAGE UndecidableInstances #-}

module Zmq.Socket
  ( Socket(..)
  , withSocket
  , CanReceive
  , CanSend
  , SocketType(..)
  , IsSocketType(..)
  ) where

import qualified GHC.TypeLits as TypeLits

import Zmq.Internal
import Zmq.Prelude
import Zmq.Util.SBool (SBool(..))
import qualified Zmq.FFI as FFI


newtype Socket ( a :: SocketType )
  = Socket
  { unSocket :: ForeignPtr () }

withSocket
  :: Socket typ
  -> ( Ptr () -> IO a )
  -> IO a
withSocket socket =
  withForeignPtr ( unSocket socket )


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

class IsSocketType typ => CanSend ( typ :: SocketType )

instance
  {-# OVERLAPPABLE #-}
  ( IsSocketType typ
  , TypeLits.TypeError
    ( 'TypeLits.Text "Cannot send on a "
      'TypeLits.:<>:
      'TypeLits.ShowType typ
      'TypeLits.:<>:
      'TypeLits.Text " socket."
    )
  )
  => CanSend typ

instance CanSend 'Pub


class IsSocketType ( typ :: SocketType ) where
  type IsThreadSafe typ :: Bool

  socketType :: CInt
  threadSafeEvidence :: SBool ( IsThreadSafe typ )

instance IsSocketType 'Pub where
  type IsThreadSafe 'Pub = 'False

  socketType :: CInt
  socketType =
    FFI.zMQ_PUB

  threadSafeEvidence =
    SFalse

instance IsSocketType 'Sub where
  type IsThreadSafe 'Sub = 'False

  socketType :: CInt
  socketType =
    FFI.zMQ_SUB

  threadSafeEvidence =
    SFalse
