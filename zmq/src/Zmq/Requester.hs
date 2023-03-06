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
    receive,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Libzmq
import Numeric.Natural (Natural)
import Zmq.Error (Error (..), catchingOkErrors)
import Zmq.Internal.Options (Options)
import Zmq.Internal.Options qualified as Options
import Zmq.Internal.Socket (CanReceive, Socket (withSocket), ThreadUnsafeSocket (..))
import Zmq.Internal.Socket qualified as Socket

-- | A __requester__ socket.
--
-- Valid peers: __replier__, __router__
newtype Requester
  = Requester ThreadUnsafeSocket
  deriving stock (Eq)
  deriving newtype (Socket)
  deriving anyclass
    ( CanReceive,
      Options.CanSetSendQueueSize
    )

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
send socket0 message = do
  catchingOkErrors do
    let loop =
          withSocket socket0 \socket ->
            Socket.sendDontWait socket message >>= \case
              False -> do
                Socket.blockUntilCanSend socket
                loop
              True -> pure ()
    loop

-- | Receive a __message__ on a __requester__ from the last peer sent to.
receive :: Requester -> IO (Either Error ByteString)
receive socket =
  catchingOkErrors do
    withSocket socket Socket.receive
