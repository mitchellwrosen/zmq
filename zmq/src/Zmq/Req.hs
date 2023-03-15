module Zmq.Req
  ( Req,
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
import Zmq.Internal.Poll (CanPoll (toPollable), Pollable (..))
import Zmq.Internal.Socket (CanReceive, CanReceives, CanSend, Socket (withSocket))
import Zmq.Internal.Socket qualified as Socket
import Zmq.Internal.ThreadUnsafeSocket (ThreadUnsafeSocket)
import Zmq.Internal.ThreadUnsafeSocket qualified as ThreadUnsafeSocket

-- | A __requester__ socket.
--
-- Valid peers: __replier__, __router__
data Req
  = Req
      !ThreadUnsafeSocket
      -- The last message we received, if any. See Note [Req message buffer] for details.
      !(IORef (Maybe (List.NonEmpty ByteString)))
  deriving stock (Eq)
  deriving anyclass
    ( Options.CanSetSendQueueSize
    )

instance CanPoll Req where
  toPollable (Req socket messageBuffer) =
    PollableREQ (ThreadUnsafeSocket.raw socket) messageBuffer

instance CanReceive Req where
  receive_ = receive

instance CanReceives Req where
  receives_ = receives

instance CanSend Req where
  send_ = send

instance Socket Req where
  openSocket = open
  getSocket (Req socket _) = ThreadUnsafeSocket.raw socket
  withSocket (Req socket _) = ThreadUnsafeSocket.with socket
  socketName (Req socket _) = ThreadUnsafeSocket.name socket

defaultOptions :: Options Req
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Req
sendQueueSize =
  Options.sendQueueSize

-- | Open a __requester__.
open :: Options Req -> IO (Either Error Req)
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
    pure (Req socket messageBuffer)

-- | Bind a __requester__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Req -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __requester__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Req -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __requester__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Req -> Text -> IO (Either Error ())
connect =
  Socket.connect

-- | Disconnect a __requester__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Req -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __requester__ to one peer (round-robin).
--
-- This operation blocks until a peer can receive the message.
--
-- /Alias/: 'Zmq.send'
send :: Req -> ByteString -> IO (Either Error ())
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
sends :: Req -> [ByteString] -> IO (Either Error ())
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
receive :: Req -> IO (Either Error ByteString)
receive socket@(Req _ messageBuffer) =
  -- Remember: this socket isn't thread safe, so we don't have to be very careful with our readIORef/writeIORefs
  readIORef messageBuffer >>= \case
    Nothing -> catchingOkErrors (Socket.receiveOne socket)
    Just (frame :| _) -> do
      writeIORef messageBuffer Nothing
      pure (Right frame)

-- | Receive a __multiframe message__ on a __requester__ from the last peer sent to.
--
-- /Alias/: 'Zmq.receives'
receives :: Req -> IO (Either Error [ByteString])
receives socket@(Req _ messageBuffer) =
  -- Remember: this socket isn't thread safe, so we don't have to be very careful with our readIORef/writeIORefs
  readIORef messageBuffer >>= \case
    Nothing ->
      catchingOkErrors do
        frame :| frames <- Socket.receiveMany socket
        pure (frame : frames)
    Just (frame :| frames) -> do
      writeIORef messageBuffer Nothing
      pure (Right (frame : frames))
