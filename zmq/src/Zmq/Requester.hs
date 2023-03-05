module Zmq.Requester
  ( Requester,
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
import Data.Coerce (coerce)
import Data.Text (Text)
import Libzmq
import Zmq.Error (Error (..), catchingOkErrors)
import Zmq.Internal.Socket (CanReceive, Socket (withSocket), ThreadUnsafeSocket (..))
import Zmq.Internal.Socket qualified as Socket

-- | A __requester__ socket.
--
-- Valid peers: __replier__, __router__
newtype Requester
  = Requester ThreadUnsafeSocket
  deriving stock (Eq)
  deriving newtype (Socket)

instance CanReceive Requester

-- | Open a __requester__.
open :: IO (Either Error Requester)
open =
  coerce (catchingOkErrors (Socket.openThreadUnsafeSocket ZMQ_REQ))

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
