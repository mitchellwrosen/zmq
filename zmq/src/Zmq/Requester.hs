module Zmq.Requester
  ( Requester,
    defaultOptions,
    sendQueueSize,
    open,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    sends,
    receive,
    receives,
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error (..), catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Poll (CanPoll (getSocketType))
import Zmq.Internal.Socket (CanReceive, CanReceives, CanSend, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadUnsafeSocket (ThreadUnsafeSocket)
import Zmq.Internal.ThreadUnsafeSocket qualified as ThreadUnsafeSocket

-- | A __requester__ socket.
--
-- Valid peers: __replier__, __router__
data Requester
  = Requester
      !ThreadUnsafeSocket
      -- The last message we received, if any. See Note [Requester message buffer] for details.
      !(IORef (Maybe (List.NonEmpty ByteString)))
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetSendQueueSize
    )

instance CanPoll Requester where
  getSocketType = ZMQ_REQ

instance CanReceive Requester where
  receive_ = receive

instance CanReceives Requester where
  receives_ = receives

instance CanSend Requester where
  send_ = send

instance Socket Requester where
  openSocket = open
  getSocket (Requester socket _) = ThreadUnsafeSocket.raw socket
  withSocket (Requester socket _) = ThreadUnsafeSocket.with socket
  socketName (Requester socket _) = ThreadUnsafeSocket.name socket

defaultOptions :: Options Requester
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Requester
sendQueueSize =
  Options.sendQueueSize

-- | Open a __requester__.
open :: Options Requester -> IO (Either Error Requester)
open options =
  catchingOkErrors do
    socket <-
      ThreadUnsafeSocket.open
        ZMQ_REQ
        ( Options.sockopt ZMQ_REQ_CORRELATE 1
            <> Options.sockopt ZMQ_REQ_RELAXED 1
            <> options
        )
    messageBuffer <- newIORef Nothing
    pure (Requester socket messageBuffer)

-- | Bind a __requester__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Requester -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __requester__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Requester -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __requester__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Requester -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __requester__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Requester -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __requester__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
--
-- /Alias/: 'Zmq.send'
send :: Requester -> ByteString -> IO (Either Error ())
send socket frame = do
  catchingOkErrors do
    let loop =
          Socket.sendOneDontWait socket frame False >>= \case
            False -> do
              Socket.blockUntilCanSend socket
              loop
            True -> pure ()
    loop

-- | Send a __multiframe message__ on a __requester__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
sends :: Requester -> [ByteString] -> IO (Either Error ())
sends socket = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      let loop = do
            sent <- Socket.sendManyDontWait socket (frame :| frames)
            when (not sent) do
              Socket.blockUntilCanSend socket
              loop
      loop

-- | Receive a __message__ on a __requester__ from the last peer sent to.
--
-- /Alias/: 'Zmq.receive'
receive :: Requester -> IO (Either Error ByteString)
receive socket@(Requester _ messageBuffer) =
  -- Remember: this socket isn't thread safe, so we don't have to be very careful with our readIORef/writeIORefs
  readIORef messageBuffer >>= \case
    Nothing -> catchingOkErrors (Socket.receiveOne socket)
    Just (frame :| _) -> do
      writeIORef messageBuffer Nothing
      pure (Right frame)

-- | Receive a __multiframe message__ on a __requester__ from the last peer sent to.
--
-- /Alias/: 'Zmq.receives'
receives :: Requester -> IO (Either Error [ByteString])
receives socket@(Requester _ messageBuffer) =
  -- Remember: this socket isn't thread safe, so we don't have to be very careful with our readIORef/writeIORefs
  readIORef messageBuffer >>= \case
    Nothing ->
      catchingOkErrors do
        frame :| frames <- Socket.receiveMany socket
        pure (frame : frames)
    Just (frame :| frames) -> do
      writeIORef messageBuffer Nothing
      pure (Right (frame : frames))
