module Zmq
  ( main
  , Options(..)
  , defaultOptions

  , Publisher
  , Subscriber
  , XPublisher
  , XSubscriber

  , BindError
  , ConnectError
  , SendError

  , Transport(..)
  , CompatibleTransport

  , Endpoint
  , inproc

  , SubscriptionMessage(..)

  , Error(..)
  , CanReturnEADDRINUSE
  , CanReturnEADDRNOTAVAIL
  , CanReturnEHOSTUNREACH
  , CanReturnEINVAL
  , CanReturnEMTHREAD
  , CanReturnENODEV

  ) where

import System.Mem (performGC)

import Zmq.API.Bind (BindError)
import Zmq.API.Connect (ConnectError)
import Zmq.API.Send (SendError)
import Zmq.Context (contextVar, setIoThreads, setMaxSockets)
import Zmq.Endpoint (Endpoint(..), inproc)
import Zmq.Error
import Zmq.Internal (CompatibleTransport, Transport(..))
import Zmq.Prelude
import Zmq.Publisher (Publisher)
import Zmq.Subscriber (Subscriber)
import Zmq.SubscriptionMessage (SubscriptionMessage(..))
import Zmq.XPublisher (XPublisher)
import Zmq.XSubscriber (XSubscriber)
import qualified Zmq.FFI as FFI


data Options
  = Options
  { ioThreads :: Natural
  , maxSockets :: Natural
  }

defaultOptions :: Options
defaultOptions =
  Options
    { ioThreads = fromIntegral FFI.zMQ_IO_THREADS_DFLT
    , maxSockets = fromIntegral FFI.zMQ_MAX_SOCKETS_DFLT
    }

-- | Run an action in the context of a global ZeroMQ context. This should wrap
-- your @main@ function, and must only be called once; functions from this
-- library that are called outside of this context will fail at runtime.
main :: Options -> IO a -> IO a
main options action =
  bracket_ setup teardown do
    context <- readMVar contextVar
    setIoThreads context ( ioThreads options )
    setMaxSockets context ( maxSockets options )
    action

  where
    setup :: IO ()
    setup =
      FFI.zmq_ctx_new >>= putMVar contextVar

    teardown :: IO ()
    teardown = do
      context <-
        takeMVar contextVar

      fix \again -> do
        performGC -- trigger socket finalizers

        FFI.zmq_ctx_term context >>= \case
          0 ->
            pure ()

          _ ->
            FFI.zmq_errno >>= \case
              EINTR_ ->
                again

              errno ->
                bugUnexpectedErrno "zmq_ctx_term" errno
