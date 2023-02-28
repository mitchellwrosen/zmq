module Zmqhs.Frame
  ( withTemporaryFrame,
    copyFrameBytes,
  )
where

import Control.Exception (bracket_)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Foreign.Marshal.Alloc (alloca)
import Libzmq qualified as Libzmq

withTemporaryFrame :: (Libzmq.Zmq_msg_t -> IO a) -> IO a
withTemporaryFrame f =
  alloca \(Libzmq.Zmq_msg_t -> message) ->
    bracket_
      (Libzmq.zmq_msg_init message)
      (Libzmq.zmq_msg_close message)
      (f message)

-- | Copy the bytes out of a frame
copyFrameBytes :: Libzmq.Zmq_msg_t -> IO ByteString
copyFrameBytes message = do
  bytes <- Libzmq.zmq_msg_data message
  size <- Libzmq.zmq_msg_size message
  ByteString.packCStringLen (bytes, size)
