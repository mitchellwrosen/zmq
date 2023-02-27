module Zmqhs.Frame
  ( Frame (..),
    withTemporaryFrame,
    isLastFrame,
    copyFrameBytes,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import Libzmq qualified
import UnliftIO

newtype Frame = Frame {unFrame :: Ptr Libzmq.Zmq_msg_t}

withTemporaryFrame :: MonadUnliftIO m => (Frame -> m a) -> m a
withTemporaryFrame f =
  unliftedAlloca \frame ->
    bracket_
      (liftIO (Libzmq.zmq_msg_init frame))
      (liftIO (Libzmq.zmq_msg_close frame))
      (f (Frame frame))

-- | Returns whether this is the last frame in the message.
isLastFrame :: MonadIO m => Frame -> m Bool
isLastFrame frame = liftIO do
  (/= 1) <$> Libzmq.zmq_msg_get (unFrame frame) Libzmq.zMQ_MORE

-- | Copy the bytes out of a frame
copyFrameBytes :: MonadIO m => Frame -> m ByteString
copyFrameBytes frame = liftIO do
  bytes <- Libzmq.zmq_msg_data (unFrame frame)
  size <- Libzmq.zmq_msg_size (unFrame frame)
  ByteString.packCStringLen (bytes, fromIntegral size)

unliftedAlloca :: (MonadUnliftIO m, Storable a) => (Ptr a -> m b) -> m b
unliftedAlloca f =
  withRunInIO \unlift ->
    alloca \ptr ->
      unlift (f ptr)
