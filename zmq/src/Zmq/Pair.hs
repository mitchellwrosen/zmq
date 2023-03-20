{-# OPTIONS_GHC -fno-warn-orphans #-}

module Zmq.Pair
  ( Pair,
    defaultOptions,
    sendQueueSize,
    open,
    open_,
    bind,
    unbind,
    connect,
    connect_,
    disconnect,
    send,
    sends,
    receive,
    receives,
  )
where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error (..), catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanReceive, CanReceives, CanSend, Socket (..))
import Zmq.Internal.Socket qualified as Socket

-- | A thread-safe __pair__ socket.
--
-- Valid peers: __pair__
type Pair =
  Socket "PAIR"

instance Options.CanSetSendQueueSize Pair

instance CanSend Pair where
  send_ = send

instance CanReceive Pair where
  receive_ = receive

instance CanReceives Pair where
  receives_ = receives

defaultOptions :: Options Pair
defaultOptions =
  Options.defaultOptions

sendQueueSize :: Natural -> Options Pair
sendQueueSize =
  Options.sendQueueSize

-- | Open a __pair__.
open :: Options Pair -> IO (Either Error Pair)
open options =
  catchingOkErrors (open_ options)

open_ :: Options Pair -> IO Pair
open_ options =
  Socket.openSocket ZMQ_PAIR options Socket.PairExtra

-- | Bind a __pair__ to an __endpoint__.
--
-- /Alias/: 'Zmq.bind'
bind :: Pair -> Text -> IO (Either Error ())
bind =
  Socket.bind

-- | Unbind a __pair__ from an __endpoint__.
--
-- /Alias/: 'Zmq.unbind'
unbind :: Pair -> Text -> IO ()
unbind =
  Socket.unbind

-- | Connect a __pair__ to an __endpoint__.
--
-- /Alias/: 'Zmq.connect'
connect :: Pair -> Text -> IO (Either Error ())
connect =
  Socket.connect

connect_ :: Pair -> Text -> IO ()
connect_ =
  Socket.connect_

-- | Disconnect a __pair__ from an __endpoint__.
--
-- /Alias/: 'Zmq.disconnect'
disconnect :: Pair -> Text -> IO ()
disconnect =
  Socket.disconnect

-- | Send a __message__ on a __pair__ to the peer.
--
-- This operation blocks until the peer can receive the message.
--
-- /Alias/: 'Zmq.send'
send :: Pair -> ByteString -> IO (Either Error ())
send socket@Socket {zsocket} frame =
  catchingOkErrors loop
  where
    loop = do
      sent <- Socket.sendOneDontWait socket frame False
      when (not sent) do
        Socket.blockUntilCanSend zsocket
        loop

-- | Send a __multiframe message__ on a __pair__ to the peer.
--
-- This operation blocks until the peer can receive the message.
sends :: Pair -> [ByteString] -> IO (Either Error ())
sends socket@Socket {zsocket} = \case
  [] -> pure (Right ())
  frame : frames -> do
    let loop = do
          sent <- Socket.sendManyDontWait socket (frame :| frames)
          when (not sent) do
            Socket.blockUntilCanSend zsocket
            loop
    catchingOkErrors loop

-- | Receive a __message__ on a __pair__ from the peer.
--
-- /Alias/: 'Zmq.receive'
receive :: Pair -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors (Socket.receiveOne socket)

-- | Receive a __multiframe message__ on a __pair__ from the peer.
--
-- /Alias/: 'Zmq.receives'
receives :: Pair -> IO (Either Error [ByteString])
receives socket =
  catchingOkErrors do
    frame :| frames <- Socket.receiveMany socket
    pure (frame : frames)
