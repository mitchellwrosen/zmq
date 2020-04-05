{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty((:|)))
import Hedgehog (Gen)
-- import Say
import Test.Hspec
import UnliftIO
import UnliftIO.Concurrent (forkIO)
import qualified Data.Text as Text
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Zmq
import qualified Zmq.Publisher as Pub
import qualified Zmq.Subscriber as Sub
import qualified Zmq.XPublisher as XPub
import qualified Zmq.XSubscriber as XSub

import qualified ZmqhsSpec

main :: IO ()
main =
  Zmq.withContext Zmq.defaultOptions \ctx ->
    hspec do
      describe "zmqhs" ( ZmqhsSpec.spec ctx )
      describe "zmq" ( spec ctx )

spec :: Zmq.Context -> Spec
spec ctx = do
  describe "Basic socket" ( basicSocketSpec someSockets ctx )
  describe "Publisher" ( publisherSpec ctx )
  describe "XPublisher" ( xpublisherSpec ctx )
  describe "XSubscriber" ( xsubscriberSpec ctx )

  it "Pubsub" do
    endpoint <- randomInproc

    pub <- Pub.open @IO ctx
    Pub.bind pub endpoint

    let threads = 200
    readyVar <- newEmptyMVar
    replicateM_ threads do
      ( forkIO . void . tryAny ) do
        readMVar readyVar
        forever ( Pub.send pub ( "" :| [] ) )

    sub <- Sub.open ctx
    Sub.connect sub endpoint
    Sub.subscribe sub ""
    putMVar readyVar ()
    replicateM_ 20000 ( Sub.recv sub )
    Pub.close pub
    Sub.close sub

basicSocketSpec :: [ SomeSocket ] -> Zmq.Context -> Spec
basicSocketSpec sockets ctx = do
  for_ sockets \SomeSocket{name, open, close, bind, unbind, connect, disconnect} -> do
    describe name do
      it "open-close" do
        sock <- open ctx
        close sock

      it "open-close-close" do
        sock <- open ctx
        close sock
        close sock

      it "bind" do
        sock <- open ctx
        endpoint <- randomInproc
        bind sock endpoint
        close sock

      it "bind-unbind" do
        sock <- open ctx
        endpoint <- randomInproc
        bind sock endpoint
        unbind sock endpoint
        close sock

      it "unbind" do
        sock <- open ctx
        endpoint <- randomInproc
        unbind sock endpoint
        close sock

      it "unbind bogus" do
        sock <- open ctx
        unbind sock ( Zmq.Tcp "" ) `shouldThrow`
          ( == Zmq.Error "zmq_unbind" Zmq.EINVAL "Invalid argument" )
        liftIO ( close sock )

      it "connect" do
        sock <- open ctx
        endpoint <- randomInproc
        connect sock endpoint
        close sock

      it "connect-disconnect" do
        sock <- open ctx
        endpoint <- randomInproc
        connect sock endpoint
        disconnect sock endpoint
        close sock

      it "disconnect" do
        sock <- open ctx
        endpoint <- randomInproc
        disconnect sock endpoint
        close sock

      it "disconnect bogus" do
        sock <- open ctx
        disconnect sock ( Zmq.Tcp "" ) `shouldThrow`
          ( == Zmq.Error "zmq_disconnect" Zmq.EINVAL "Invalid argument" )
        close sock

publisherSpec :: Zmq.Context -> Spec
publisherSpec ctx =
  it "send" do
    pub <- Pub.open @IO ctx
    Pub.send pub ( "" :| [] )
    Pub.close pub

xpublisherSpec :: Zmq.Context -> Spec
xpublisherSpec ctx =
  it "receives explicit subscription messages" do
    endpoint <- randomInproc
    xpub <- XPub.open ctx
    sub <- Sub.open ctx
    XPub.bind xpub endpoint
    Sub.connect sub endpoint
    Sub.subscribe sub "hi"
    XPub.recv xpub `shouldReturn` Zmq.Subscribe "hi"
    Sub.unsubscribe sub "hi"
    XPub.recv xpub `shouldReturn` Zmq.Unsubscribe "hi"
    XPub.close xpub
    Sub.close sub

xsubscriberSpec :: Zmq.Context -> Spec
xsubscriberSpec ctx =
  it "can subscribe to a publisher" do
    endpoint <- randomInproc
    pub <- Pub.open ctx
    xsub <- XSub.open ctx
    Pub.bind pub endpoint
    XSub.connect xsub endpoint
    XSub.subscribe xsub ""
    ( void . forkIO . void . tryAny . forever ) do
      Pub.send pub ( "hi" :| [] )
    XSub.recv xsub `shouldReturn` "hi" :| []
    Pub.close pub
    XSub.close xsub


data SomeSocket
  = forall socket.
    SomeSocket
  { name :: String
  , open :: Zmq.Context -> IO socket
  , close :: socket -> IO ()
  , bind :: forall transport. socket -> Zmq.Endpoint transport -> IO ()
  , unbind :: forall transport. socket -> Zmq.Endpoint transport -> IO ()
  , connect :: forall transport. socket -> Zmq.Endpoint transport -> IO ()
  , disconnect :: forall transport. socket -> Zmq.Endpoint transport -> IO ()
  }

someSockets :: [ SomeSocket ]
someSockets =
  [ SomeSocket
      { name = "Publisher"
      , open = \ctx -> liftIO ( Pub.open ctx )
      , close = \sock -> liftIO ( Pub.close sock )
      , bind = \sock endpoint -> liftIO ( Pub.bind sock endpoint )
      , unbind = \sock endpoint -> liftIO ( Pub.unbind sock endpoint )
      , connect = \sock endpoint -> liftIO ( Pub.connect sock endpoint )
      , disconnect = \sock endpoint -> liftIO ( Pub.disconnect sock endpoint )
      }
  , SomeSocket
      { name = "Subscriber"
      , open = \ctx -> liftIO ( Sub.open ctx )
      , close = \sock -> liftIO ( Sub.close sock )
      , bind = \sock endpoint -> liftIO ( Sub.bind sock endpoint )
      , unbind = \sock endpoint -> liftIO ( Sub.unbind sock endpoint )
      , connect = \sock endpoint -> liftIO ( Sub.connect sock endpoint )
      , disconnect = \sock endpoint -> liftIO ( Sub.disconnect sock endpoint )
      }
  , SomeSocket
      { name = "XPublisher"
      , open = \ctx -> liftIO ( XPub.open ctx )
      , close = \sock -> liftIO ( XPub.close sock )
      , bind = \sock endpoint -> liftIO ( XPub.bind sock endpoint )
      , unbind = \sock endpoint -> liftIO ( XPub.unbind sock endpoint )
      , connect = \sock endpoint -> liftIO ( XPub.connect sock endpoint )
      , disconnect = \sock endpoint -> liftIO ( XPub.disconnect sock endpoint )
      }
  , SomeSocket
      { name = "XSubscriber"
      , open = \ctx -> liftIO ( XSub.open ctx )
      , close = \sock -> liftIO ( XSub.close sock )
      , bind = \sock endpoint -> liftIO ( XSub.bind sock endpoint )
      , unbind = \sock endpoint -> liftIO ( XSub.unbind sock endpoint )
      , connect = \sock endpoint -> liftIO ( XSub.connect sock endpoint )
      , disconnect = \sock endpoint -> liftIO ( XSub.disconnect sock endpoint )
      }
  ]


--------------------------------------------------------------------------------

openPubSub :: Zmq.Context -> IO ( Zmq.Publisher, Zmq.Subscriber )
openPubSub ctx = do
  endpoint <- randomInproc
  pub <- liftIO ( Pub.open ctx )
  sub <- liftIO ( Sub.open ctx )
  liftIO ( Pub.bind pub endpoint )
  liftIO ( Sub.connect sub endpoint )
  pure ( pub, sub )


--------------------------------------------------------------------------------

randomInproc :: MonadIO m => m ( Zmq.Endpoint 'Zmq.TransportInproc )
randomInproc =
  Gen.sample ( Gen.prune genInproc )

randomTcp :: MonadIO m => m ( Zmq.Endpoint 'Zmq.TransportTcp )
randomTcp =
  Gen.sample genTcp

genInproc :: Gen ( Zmq.Endpoint 'Zmq.TransportInproc )
genInproc = do
  name <- Gen.text ( Range.linear 100 200 ) Gen.unicode
  case Zmq.inproc name of
    Nothing -> error ( "bad inproc generator: " ++ show name )
    Just endpoint -> pure endpoint

genTcp :: Gen ( Zmq.Endpoint 'Zmq.TransportTcp )
genTcp = do
  port <- Gen.prune ( Gen.int ( Range.constant 49152 65535 ) )
  pure ( Zmq.Tcp ( "127.0.0.1:" <> Text.pack ( show port ) ) )
