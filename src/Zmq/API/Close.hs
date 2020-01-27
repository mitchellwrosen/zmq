module Zmq.API.Close
  ( close
  ) where

import Foreign.ForeignPtr (finalizeForeignPtr)

import Zmq.Prelude
import Zmq.Socket


close
  :: MonadIO m
  => Socket typ
  -> m ()
close socket =
  liftIO ( closeIO socket )

closeIO
  :: Socket typ
  -> IO ()
closeIO socket =
  finalizeForeignPtr ( unSocket socket )
