module Zmq.Internal.Socket
  ( with,
    bind,
    unbind,
    connect,
    disconnect,
    send,
    receive,
  )
where

import Control.Concurrent.MVar
import Control.Exception
import Data.ByteString (ByteString)
import Data.Functor (void, (<&>))
import Data.List.NonEmpty (NonEmpty)
import Libzmq qualified
import Zmq.Context
import Zmq.Endpoint
import Zmq.Error
import Zmq.Internal (renderEndpoint)
import Zmqhs qualified

with :: Context -> (MVar Libzmq.Zmq_socket_t -> IO (Either Error a)) -> IO (Either Error a)
with context action =
  mask \restore ->
    open context >>= \case
      Left err -> pure (Left err)
      Right socket -> do
        socketVar <- newMVar socket
        try (restore (action socketVar)) >>= \case
          Left (exception :: SomeException) -> do
            uninterruptibleMask_ (void (close socket))
            throwIO exception
          Right result -> do
            uninterruptibleMask_ (close socket) <&> \case
              Left err ->
                Left case result of
                  Left err0 -> err0 -- prefer user's error to close error
                  Right _ -> err
              Right () -> result

open :: Context -> IO (Either Error Libzmq.Zmq_socket_t)
open (Context context) =
  Libzmq.zmq_socket context Libzmq.ZMQ_SUB <&> \case
    Left err -> Left (enrichError "zmq_socket" err)
    Right socket -> Right socket

close :: Libzmq.Zmq_socket_t -> IO (Either Error ())
close socket =
  Libzmq.zmq_close socket <&> \case
    Left err -> Left (enrichError "zmq_close" err)
    Right () -> Right ()

bind :: MVar Libzmq.Zmq_socket_t -> Endpoint transport -> IO (Either Error ())
bind socketVar endpoint =
  withMVar socketVar \socket ->
    enrichFunction "zmq_bind" (Libzmq.zmq_bind socket (renderEndpoint endpoint))

unbind :: MVar Libzmq.Zmq_socket_t -> Endpoint transport -> IO (Either Error ())
unbind socketVar endpoint =
  withMVar socketVar \socket ->
    enrichFunction "zmq_unbind" (Libzmq.zmq_unbind socket (renderEndpoint endpoint))

connect :: MVar Libzmq.Zmq_socket_t -> Endpoint transport -> IO (Either Error ())
connect socketVar endpoint =
  withMVar socketVar \socket ->
    enrichFunction "zmq_connect" (Libzmq.zmq_connect socket (renderEndpoint endpoint))

disconnect :: MVar Libzmq.Zmq_socket_t -> Endpoint transport -> IO (Either Error ())
disconnect socketVar endpoint =
  withMVar socketVar \socket ->
    enrichFunction "zmq_disconnect" (Libzmq.zmq_disconnect socket (renderEndpoint endpoint))

send :: MVar Libzmq.Zmq_socket_t -> NonEmpty ByteString -> IO ()
send socketVar message =
  withMVar socketVar \socket ->
    Zmqhs.send socket message

receive :: MVar Libzmq.Zmq_socket_t -> IO (NonEmpty ByteString)
receive socketVar =
  withMVar socketVar Zmqhs.receive
