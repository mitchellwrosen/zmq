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

import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error (..), catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanPoll, CanReceive, Socket (withSocket), ThreadUnsafeSocket (..))
import Zmq.Internal.Socket qualified as Socket

-- | A __requester__ socket.
--
-- Valid peers: __replier__, __router__
newtype Requester
  = Requester ThreadUnsafeSocket
  deriving stock (Eq)
  deriving newtype (Socket)
  deriving anyclass
    ( CanPoll,
      Options.CanSetSendQueueSize
    )

instance CanReceive Requester where
  receive_ = receive

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
    socket@(ThreadUnsafeSocket zsocket _) <- Socket.openThreadUnsafeSocket ZMQ_REQ
    Options.setSocketOption zsocket ZMQ_REQ_CORRELATE 1
    Options.setSocketOption zsocket ZMQ_REQ_RELAXED 1
    Options.setSocketOptions zsocket ZMQ_REQ options
    pure (Requester socket)

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
send :: Requester -> ByteString -> IO (Either Error ())
send socket0 frame = do
  catchingOkErrors do
    withSocket socket0 \socket -> do
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
sends socket0 = \case
  [] -> pure (Right ())
  frame : frames ->
    catchingOkErrors do
      withSocket socket0 \socket -> do
        let loop =
              Socket.sendManyDontWait socket (frame :| frames) >>= \case
                False -> do
                  Socket.blockUntilCanSend socket
                  loop
                True -> pure ()
        loop

-- | Receive a __message__ on a __requester__ from the last peer sent to.
--
-- /Alias/: 'Zmq.receive'
receive :: Requester -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors do
    withSocket socket Socket.receiveOne

-- | Receive a __multiframe message__ on a __requester__ from the last peer sent to.
receives :: Requester -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- withSocket socket Socket.receiveMany
    pure (frame : frames)
