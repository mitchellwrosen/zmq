module Zmq.API.Send
  ( sendThatNeverBlocks
  ) where

import Data.Bits ((.|.))
import qualified Data.ByteString.Unsafe as ByteString

import qualified Libzmq
import qualified Zmq.FFI as FFI

import qualified Zmqhs

import Zmq.Exception
import Zmq.Prelude


-- | Send on a socket that is guaranteed not to block in C (like PUB).
sendThatNeverBlocks
  :: Zmqhs.Socket
  -> NonEmpty ByteString
  -> IO ()
sendThatNeverBlocks socket =
  sendThatNeverBlocks_ socket . toList

sendThatNeverBlocks_
  :: Zmqhs.Socket
  -> [ ByteString ] -- Invariant: non-empty
  -> IO ()
sendThatNeverBlocks_ socket =
  fix \loop -> \case
    [ message ] ->
      sendThatNeverBlocks__ socket message 0

    message : messages -> do
      sendThatNeverBlocks__ socket message ( Libzmq.dontwait .|. Libzmq.sndmore )
      loop messages

    [] ->
      error "sendThatNeverBlocks_: []"

sendThatNeverBlocks__
  :: Zmqhs.Socket
  -> ByteString
  -> CInt
  -> IO ()
sendThatNeverBlocks__ socket message flags =
  ByteString.unsafeUseAsCStringLen message \( ptr, fromIntegral -> len ) ->
    FFI.zmq_send ( Zmqhs.unSocket socket ) ptr len flags >>= \case
      -1 ->
        Libzmq.errno >>= exception "zmq_send"

      -- Ignore number of bytes sent; why is this interesting?
      _ ->
        pure ()
