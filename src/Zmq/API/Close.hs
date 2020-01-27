module Zmq.API.Close
  ( close
  , closeIO
  ) where

import Foreign.ForeignPtr (finalizeForeignPtr)

import Zmq.Prelude
import Zmq.Socket


-- | <http://api.zeromq.org/4-3:zmq-close>
--
-- Promptly close a socket. It is not strictly necessary to call this function;
-- sockets are closed automatically when garbage collected.
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
