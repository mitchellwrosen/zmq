module Zmq.API.Close
  ( close
  ) where

import Foreign.ForeignPtr (finalizeForeignPtr)

import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | <http://api.zeromq.org/4-3:zmq-close>
--
-- Promptly close a socket. It is not strictly necessary to call this function;
-- sockets are closed automatically when garbage collected.
close
  :: ForeignPtr FFI.Socket
  -> IO ()
close =
  finalizeForeignPtr
