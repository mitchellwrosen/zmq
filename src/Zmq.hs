module Zmq
  ( CanReturnEMFILE
  , Error(..)
  , Function(..)
  , Socket
  , SocketError
  , socket
    -- * Re-exports
  , Zmq.SocketType
  , Zmq.Sub
  ) where

import Control.Exception (mask)
import Data.Coerce (coerce)
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import qualified System.ZMQ4 as Zmq
import qualified System.ZMQ4.Internal as Zmq
import qualified System.ZMQ4.Internal.Base as Zmq

-- | Global context.
context :: Zmq.ZMQCtx
context =
  unsafePerformIO ( coerce Zmq.c_zmq_ctx_new )
{-# NOINLINE context #-}

newtype Socket a
  = Socket
  { unSocket :: ForeignPtr () }

data Function
  = Function'Socket

data Error ( function :: Function ) where
  EMFILE :: ( CanReturnEMFILE function ~ 'True ) => Error function

instance Show ( Error function ) where
  show = \case
    EMFILE -> "EMFILE"

type family CanReturnEMFILE ( function :: Function ) :: Bool where
  CanReturnEMFILE 'Function'Socket = 'True

type SocketError
  = Error 'Function'Socket

socket
  :: Zmq.SocketType a
  => IO ( Either SocketError ( Socket a ) )
socket =
  socket_

socket_
  :: forall a.
     Zmq.SocketType a
  => IO ( Either SocketError ( Socket a ) )
socket_ =
  mask \unmask -> do
    ptr :: Ptr () <-
      Zmq.c_zmq_socket context ( socketType @a )

    if ptr == nullPtr
      then
        unmask do
          errno >>= \case
            EMFILE_ ->
              pure ( Left EMFILE )

            n ->
              unexpectedErrno "socket" n

      else do
        foreignPtr :: ForeignPtr () <-
          newForeignPtr zmq_close ptr

        unmask ( pure ( Right ( coerce foreignPtr ) ) )

socketType :: forall a. Zmq.SocketType a => CInt
socketType =
  coerce ( Zmq.zmqSocketType ( undefined :: a ) )

errno :: IO Errno
errno =
  coerce Zmq.c_zmq_errno

unexpectedErrno :: String -> Errno -> a
unexpectedErrno message ( Errno n ) =
  error ( message ++ ": unexpected errno " ++ show n )

pattern EMFILE_ :: Errno
pattern EMFILE_ <- ((== eMFILE) -> True)

foreign import ccall unsafe "&zmq_close"
  zmq_close :: FunPtr ( Ptr () -> IO () )
