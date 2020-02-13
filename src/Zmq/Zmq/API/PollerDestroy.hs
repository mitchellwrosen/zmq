module Zmq.API.PollerDestroy where
{-
module Zmq.API.PollerDestroy
  ( pollerDestroy
  ) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (poke)

import Zmq.Error
import Zmq.Poller
import Zmq.Prelude
import qualified Zmq.FFI as FFI


-- | Destroy a 'Poller'.
pollerDestroy
  :: MonadIO m
  => Poller
  -> m ()
pollerDestroy poller =
  liftIO ( pollerDestroyIO poller )

pollerDestroyIO
  :: Poller
  -> IO ()
pollerDestroyIO poller =
  alloca \poller_ptr -> do
    poke poller_ptr ( unPoller poller )

    FFI.zmq_poller_destroy poller_ptr >>= \case
      0 ->
        pure ()

      _ ->
        Libzmq.errno <&> \case
          -- Poller pointer did not point to a valid pointer. Eh, ignore it.
          EFAULT_ -> ()

          errno -> bugUnexpectedErrno "zmq_poller_destroy" errno
-}
