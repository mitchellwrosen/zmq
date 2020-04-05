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

    Pub.with @IO ctx \pub -> do
      Pub.bind pub endpoint

      let threads = 200
      readyVar <- newEmptyMVar
      replicateM_ threads do
        ( forkIO . void . tryAny ) do
          readMVar readyVar
          forever ( Pub.send pub ( "" :| [] ) )

      Sub.with ctx \sub -> do
        Sub.connect sub endpoint
        Sub.subscribe sub ""
        putMVar readyVar ()
        replicateM_ 20000 ( Sub.recv sub )

basicSocketSpec :: [ SomeSocket ] -> Zmq.Context -> Spec
basicSocketSpec sockets ctx = do
  for_ sockets \SomeSocket{name, with, close, bind, unbind, connect, disconnect} -> do
    describe name do
      it "open-close" do
        with ctx \_ ->
          pure ()

      it "open-close-close" do
        with ctx \sock ->
          close sock

      it "bind" do
        with ctx \sock -> do
          endpoint <- randomInproc
          bind sock endpoint

      it "bind-unbind" do
        with ctx \sock -> do
          endpoint <- randomInproc
          bind sock endpoint
          unbind sock endpoint

      it "unbind" do
        with ctx \sock -> do
          endpoint <- randomInproc
          unbind sock endpoint

      it "unbind bogus" do
        with ctx \sock -> do
          unbind sock ( Zmq.Tcp "" ) `shouldThrow`
            ( == Zmq.Error "zmq_unbind" Zmq.EINVAL "Invalid argument" )

      it "connect" do
        with ctx \sock -> do
          endpoint <- randomInproc
          connect sock endpoint

      it "connect-disconnect" do
        with ctx \sock -> do
          endpoint <- randomInproc
          connect sock endpoint
          disconnect sock endpoint

      it "disconnect" do
        with ctx \sock -> do
          endpoint <- randomInproc
          disconnect sock endpoint

      it "disconnect bogus" do
        with ctx \sock ->
          disconnect sock ( Zmq.Tcp "" ) `shouldThrow`
            ( == Zmq.Error "zmq_disconnect" Zmq.EINVAL "Invalid argument" )

publisherSpec :: Zmq.Context -> Spec
publisherSpec ctx =
  it "send" do
    Pub.with @IO ctx \pub ->
      Pub.send pub ( "" :| [] )

xpublisherSpec :: Zmq.Context -> Spec
xpublisherSpec ctx =
  it "receives explicit subscription messages" do
    endpoint <- randomInproc
    XPub.with ctx \xpub ->
      Sub.with ctx \sub -> do
        XPub.bind xpub endpoint
        Sub.connect sub endpoint
        Sub.subscribe sub "hi"
        XPub.recv xpub `shouldReturn` Zmq.Subscribe "hi"
        Sub.unsubscribe sub "hi"
        XPub.recv xpub `shouldReturn` Zmq.Unsubscribe "hi"

xsubscriberSpec :: Zmq.Context -> Spec
xsubscriberSpec ctx =
  it "can subscribe to a publisher" do
    endpoint <- randomInproc
    Pub.with ctx \pub ->
      XSub.with ctx \xsub -> do
        Pub.bind pub endpoint
        XSub.connect xsub endpoint
        XSub.subscribe xsub ""
        ( void . forkIO . void . tryAny . forever ) do
          Pub.send pub ( "hi" :| [] )
        XSub.recv xsub `shouldReturn` "hi" :| []


data SomeSocket
  = forall socket.
    SomeSocket
  { name :: String
  , with :: forall a. Zmq.Context -> ( socket -> IO a ) -> IO a
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
      , with = Pub.with
      , close = Pub.close
      , bind = Pub.bind
      , unbind = Pub.unbind
      , connect = Pub.connect
      , disconnect = Pub.disconnect
      }
  , SomeSocket
      { name = "Subscriber"
      , with = Sub.with
      , close = Sub.close
      , bind = Sub.bind
      , unbind = Sub.unbind
      , connect = Sub.connect
      , disconnect = Sub.disconnect
      }
  , SomeSocket
      { name = "XPublisher"
      , with = XPub.with
      , close = XPub.close
      , bind = XPub.bind
      , unbind = XPub.unbind
      , connect = XPub.connect
      , disconnect = XPub.disconnect
      }
  , SomeSocket
      { name = "XSubscriber"
      , with = XSub.with
      , close = XSub.close
      , bind = XSub.bind
      , unbind = XSub.unbind
      , connect = XSub.connect
      , disconnect = XSub.disconnect
      }
  ]


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
