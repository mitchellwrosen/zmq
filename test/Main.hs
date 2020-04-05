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
  hspec do
    around
      ( Zmq.withContext Zmq.defaultOptions )
      ( do
          describe "zmqhs" ZmqhsSpec.spec
          describe "zmq" spec
      )

spec :: SpecWith Zmq.Context
spec = do
  describe "Basic socket" ( basicSocketSpec someSockets )
  describe "Publisher" publisherSpec
  describe "XPublisher" xpublisherSpec
  describe "XSubscriber" xsubscriberSpec

  it "Pubsub" \ctx -> do
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
        replicateM_ 20000 ( Sub.receive sub )

basicSocketSpec :: [ SomeSocket ] -> SpecWith Zmq.Context
basicSocketSpec sockets = do
  for_ sockets \SomeSocket{name, with, close, bind, unbind, connect, disconnect} -> do
    describe name do
      it "open-close" \ctx ->
        with ctx \_ ->
          pure ()

      it "open-close-close" \ctx ->
        with ctx \sock ->
          close sock

      it "bind" \ctx ->
        with ctx \sock -> do
          endpoint <- randomInproc
          bind sock endpoint

      it "bind-unbind" \ctx ->
        with ctx \sock -> do
          endpoint <- randomInproc
          bind sock endpoint
          unbind sock endpoint

      it "unbind" \ctx ->
        with ctx \sock -> do
          endpoint <- randomInproc
          unbind sock endpoint

      it "unbind bogus" \ctx ->
        with ctx \sock ->
          unbind sock ( Zmq.Tcp "" ) `shouldThrow`
            ( == Zmq.Error "zmq_unbind" Zmq.EINVAL "Invalid argument" )

      it "connect" \ctx ->
        with ctx \sock -> do
          endpoint <- randomInproc
          connect sock endpoint

      it "connect-disconnect" \ctx ->
        with ctx \sock -> do
          endpoint <- randomInproc
          connect sock endpoint
          disconnect sock endpoint

      it "disconnect" \ctx ->
        with ctx \sock -> do
          endpoint <- randomInproc
          disconnect sock endpoint

      it "disconnect bogus" \ctx ->
        with ctx \sock ->
          disconnect sock ( Zmq.Tcp "" ) `shouldThrow`
            ( == Zmq.Error "zmq_disconnect" Zmq.EINVAL "Invalid argument" )

publisherSpec :: SpecWith Zmq.Context
publisherSpec =
  it "send" \ctx ->
    Pub.with @IO ctx \pub ->
      Pub.send pub ( "" :| [] )

xpublisherSpec :: SpecWith Zmq.Context
xpublisherSpec =
  it "receives explicit subscription messages" \ctx -> do
    endpoint <- randomInproc
    XPub.with ctx \xpub ->
      Sub.with ctx \sub -> do
        XPub.bind xpub endpoint
        Sub.connect sub endpoint
        Sub.subscribe sub "hi"
        XPub.receive xpub `shouldReturn` Zmq.Subscribe "hi"
        Sub.unsubscribe sub "hi"
        XPub.receive xpub `shouldReturn` Zmq.Unsubscribe "hi"

xsubscriberSpec :: SpecWith Zmq.Context
xsubscriberSpec =
  it "can subscribe to a publisher" \ctx -> do
    endpoint <- randomInproc
    Pub.with ctx \pub ->
      XSub.with ctx \xsub -> do
        Pub.bind pub endpoint
        XSub.connect xsub endpoint
        XSub.subscribe xsub ""
        ( void . forkIO . void . tryAny . forever ) do
          Pub.send pub ( "hi" :| [] )
        XSub.receive xsub `shouldReturn` "hi" :| []


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
