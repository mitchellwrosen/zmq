module Zmqhs.Frame
  ( Frame(..)
  , withTemporaryFrame
  , isLastFrame
  , copyFrameBytes
  ) where

import Data.ByteString (ByteString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)
import UnliftIO
import qualified Data.ByteString as ByteString

import qualified Libzmq


newtype Frame
  = Frame { unFrame :: Ptr Libzmq.Frame }

withTemporaryFrame :: MonadUnliftIO m => ( Frame -> m a ) -> m a
withTemporaryFrame f =
  unliftedAlloca \frame ->
    bracket_
      ( liftIO ( Libzmq.initializeFrame frame ) )
      ( liftIO ( Libzmq.closeFrame frame ) )
      ( f ( Frame frame ) )

-- | Returns whether this is the last frame in the message.
isLastFrame :: MonadIO m => Frame -> m Bool
isLastFrame frame = liftIO do
  ( /= 1 ) <$> Libzmq.getFrameProperty ( unFrame frame ) Libzmq.zMQ_MORE

-- | Copy the bytes out of a frame
copyFrameBytes :: MonadIO m => Frame -> m ByteString
copyFrameBytes frame = liftIO do
  bytes <- Libzmq.getFrameData ( unFrame frame )
  size <- Libzmq.getFrameSize ( unFrame frame )
  ByteString.packCStringLen ( bytes, fromIntegral size )

unliftedAlloca :: ( MonadUnliftIO m, Storable a ) => ( Ptr a -> m b ) -> m b
unliftedAlloca f =
  withRunInIO \unlift ->
    alloca \ptr ->
      unlift ( f ptr )
