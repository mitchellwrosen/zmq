module Zmq.Subscriber
  ( Subscriber

  , open
  , close

  , bind
  , unbind

  , connect
  , disconnect

  , subscribe

  , recv
  ) where

import Zmq.Endpoint
import Zmq.Prelude
import Zmq.Socket
import qualified Zmq.API.Bind as API
import qualified Zmq.API.Close as API
import qualified Zmq.API.Connect as API
import qualified Zmq.API.Disconnect as API
import qualified Zmq.API.Recv as API
import qualified Zmq.API.Socket as API
import qualified Zmq.API.Subscribe as API
import qualified Zmq.API.Unbind as API
import qualified Zmq.FFI as FFI


newtype Subscriber
  = Subscriber { unSubscriber :: ForeignPtr FFI.Socket }
  deriving stock ( Eq, Data, Ord, Show )

open
  :: MonadIO m
  => m ( Maybe Subscriber )
open =
  liftIO ( coerce ( API.socketIO' FFI.zMQ_SUB ) )

close
  :: MonadIO m
  => Subscriber
  -> m ()
close subscriber =
  liftIO ( coerce API.closeIO subscriber )

bind
  :: MonadIO m
  => Subscriber
  -> Endpoint transport
  -> m ( Either API.BindError () )
bind subscriber endpoint =
  liftIO ( coerce API.bindIO' subscriber endpoint )

unbind
  :: MonadIO m
  => Subscriber
  -> Endpoint transport
  -> m ()
unbind subscriber endpoint =
  liftIO ( coerce API.unbindIO' subscriber endpoint )

connect
  :: MonadIO m
  => Subscriber
  -> Endpoint transport
  -> m ( Either API.ConnectError () )
connect subscriber endpoint =
  liftIO ( coerce API.connectIO' subscriber endpoint )

disconnect
  :: MonadIO m
  => Subscriber
  -> Endpoint transport
  -> m ()
disconnect subscriber endpoint =
  liftIO ( coerce API.disconnectIO' subscriber endpoint )

subscribe
  :: MonadIO m
  => Subscriber
  -> ByteString
  -> m ()
subscribe subscriber prefix =
  liftIO ( coerce API.subscribeIO' subscriber prefix )

recv
  :: MonadIO m
  => Subscriber
  -> m ( NonEmpty ByteString )
recv subscriber =
  liftIO ( coerce API.nonThreadsafeRecv subscriber )
