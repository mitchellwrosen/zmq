{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog hiding (failure, test)
-- import Say
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import UnliftIO.Concurrent
import UnliftIO.Exception
import qualified Data.Text as Text
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Zmq
import qualified Zmq.Publisher as Pub
import qualified Zmq.Subscriber as Sub
import qualified Zmq.XPublisher as Xpub
import qualified Zmq.XSubscriber as Xsub

main :: IO ()
main = do
  bracket
    ( Zmq.newContext Zmq.defaultOptions )
    Zmq.terminateContext
    ( \ctx -> defaultMain ( testGroup "tests" ( tests ctx ) ) )

tests :: Zmq.Context -> [ TestTree ]
tests ctx =
  [ testGroup "Publisher tests" ( publisherTests ctx )
  , testGroup "Subscriber tests" ( subscriberTests ctx )

  , test "Pubsub" do
      ( pub, sub ) <- openPubSub ctx
      Sub.subscribe sub "a"
      liftIO ( Pub.send pub ( "b" :| [] ) )
      let message = "a" :| []
      liftIO ( Pub.send pub message )
      Sub.recv sub >>= ( === message )
      liftIO ( Pub.close pub )
      Sub.close sub

  , test "Concurrent publisher" do
      endpoint <- randomInproc

      pub <- liftIO ( Pub.open ctx )
      liftIO ( Pub.bind pub endpoint )

      let threads = 200
      readyVar <- newEmptyMVar
      liftIO do
        replicateM_ threads do
          mask \restore ->
            ( forkIO . void . try @_ @SomeException . restore ) do
              readMVar readyVar
              forever ( Pub.send pub ( "" :| [] ) )

      sub <- Sub.open ctx
      Sub.connect sub endpoint
      Sub.subscribe sub ""
      putMVar readyVar ()
      replicateM_ 20000 ( Sub.recv sub )
      liftIO ( Pub.close pub )
      Sub.close sub

  , test "XPublisher recv subscription" do
      endpoint <- randomInproc
      xpub <- Xpub.open ctx
      sub <- Sub.open ctx
      Xpub.bind xpub endpoint
      Sub.connect sub endpoint
      Sub.subscribe sub "hi"
      message <- Xpub.recv xpub
      message === Zmq.Subscribe "hi"
      Xpub.close xpub
      Sub.close sub

  , test "XSubscriber send subscription" do
      endpoint <- randomInproc
      pub <- liftIO ( Pub.open ctx )
      xsub <- Xsub.open ctx
      liftIO ( Pub.bind pub endpoint )
      Xsub.connect xsub endpoint
      Xsub.subscribe xsub ""
      liftIO ( Pub.send pub ( "hi" :| [] ) )
      message <- Xsub.recv xsub
      message === "hi" :| []
      liftIO ( Pub.close pub )
      Xsub.close xsub
  ]

publisherTests :: Zmq.Context -> [ TestTree ]
publisherTests ctx =
  [ test "open-close" do
      pub <- liftIO ( Pub.open ctx )
      liftIO ( Pub.close pub )

  , test "open-close-close" do
      pub <- liftIO ( Pub.open ctx )
      liftIO ( Pub.close pub )
      liftIO ( Pub.close pub )

  , test "bind" do
      pub <- liftIO ( Pub.open ctx )
      endpoint <- randomInproc
      liftIO ( Pub.bind pub endpoint )
      liftIO ( Pub.close pub )

  , test "bind-unbind" do
      pub <- liftIO ( Pub.open ctx )
      endpoint <- randomInproc
      liftIO ( Pub.bind pub endpoint )
      liftIO ( Pub.unbind pub endpoint )
      liftIO ( Pub.close pub )

  , test "unbind" do
      pub <- liftIO ( Pub.open ctx )
      endpoint <- randomInproc
      liftIO ( Pub.unbind pub endpoint )
      liftIO ( Pub.close pub )

  , test "unbind bogus" do
      pub <- liftIO ( Pub.open ctx )
      liftIO ( Pub.unbind pub ( Zmq.Tcp "" ) ) `throws`
        Zmq.Error "zmq_unbind" 22 "Invalid argument"
      liftIO ( Pub.close pub )

  , test "connect" do
      pub <- liftIO ( Pub.open ctx )
      endpoint <- randomInproc
      liftIO ( Pub.connect pub endpoint )
      liftIO ( Pub.close pub )

  , test "connect-disconnect" do
      pub <- liftIO ( Pub.open ctx )
      endpoint <- randomInproc
      liftIO ( Pub.connect pub endpoint )
      liftIO ( Pub.disconnect pub endpoint )
      liftIO ( Pub.close pub )

  , test "disconnect" do
      pub <- liftIO ( Pub.open ctx )
      endpoint <- randomInproc
      liftIO ( Pub.disconnect pub endpoint )
      liftIO ( Pub.close pub )

  , test "disconnect bogus" do
      pub <- liftIO ( Pub.open ctx )
      liftIO ( Pub.disconnect pub ( Zmq.Tcp "" ) ) `throws`
        Zmq.Error "zmq_disconnect" 22 "Invalid argument"
      liftIO ( Pub.close pub )

  , test "send" do
      pub <- liftIO ( Pub.open ctx )
      liftIO ( Pub.send pub ( "" :| [] ) )
      liftIO ( Pub.close pub )
  ]

subscriberTests :: Zmq.Context -> [ TestTree ]
subscriberTests ctx =
  [ test "open-close" do
      sub <- Sub.open ctx
      Sub.close sub

  , test "open-close-close" do
      sub <- Sub.open ctx
      Sub.close sub
      Sub.close sub

  , test "bind" do
      sub <- Sub.open ctx
      endpoint <- randomInproc
      Sub.bind sub endpoint
      Sub.close sub

  , test "bind-unbind" do
      sub <- Sub.open ctx
      endpoint <- randomInproc
      Sub.bind sub endpoint
      Sub.unbind sub endpoint
      Sub.close sub

  , test "unbind" do
      sub <- Sub.open ctx
      endpoint <- randomInproc
      Sub.unbind sub endpoint
      Sub.close sub

  , test "unbind bogus" do
      sub <- Sub.open ctx
      Sub.unbind sub ( Zmq.Tcp "" ) `throws`
        Zmq.Error "zmq_unbind" 22 "Invalid argument"
      Sub.close sub

  , test "connect" do
      sub <- Sub.open ctx
      endpoint <- randomInproc
      Sub.connect sub endpoint
      Sub.close sub

  , test "connect-disconnect" do
      sub <- Sub.open ctx
      endpoint <- randomInproc
      Sub.connect sub endpoint
      Sub.disconnect sub endpoint
      Sub.close sub

  , test "disconnect" do
      sub <- Sub.open ctx
      endpoint <- randomInproc
      Sub.disconnect sub endpoint
      Sub.close sub

  , test "disconnect bogus" do
      sub <- Sub.open ctx
      Sub.disconnect sub ( Zmq.Tcp "" ) `throws`
        Zmq.Error "zmq_disconnect" 22 "Invalid argument"
      Sub.close sub
  ]


--------------------------------------------------------------------------------

openPubSub
  :: ( MonadIO m, MonadTest m )
  => Zmq.Context
  -> m ( Zmq.Publisher, Zmq.Subscriber )
openPubSub ctx = do
  endpoint <- randomInproc
  pub <- liftIO ( Pub.open ctx )
  sub <- Sub.open ctx
  liftIO ( Pub.bind pub endpoint )
  Sub.connect sub endpoint
  pure ( pub, sub )


--------------------------------------------------------------------------------

randomInproc
  :: MonadIO m
  => m ( Zmq.Endpoint 'Zmq.TransportInproc )
randomInproc =
  Gen.sample ( Gen.prune genInproc )

randomTcp
  :: MonadIO m
  => m ( Zmq.Endpoint 'Zmq.TransportTcp )
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


--------------------------------------------------------------------------------

test
  :: TestName
  -> TestT IO ()
  -> TestTree
test name =
  testProperty name . withTests 1 . property . Hedgehog.test

prop
  :: TestName
  -> PropertyT IO ()
  -> TestTree
prop name =
  testProperty name . property

failure
  :: ( HasCallStack, MonadTest m, Show a )
  => a
  -> m b
failure x = do
  annotate ( show x )
  Hedgehog.failure

matches
  :: ( HasCallStack, MonadTest m, Show context, Show s )
  => Prism' s a
  -> context
  -> s
  -> m a
matches p context s =
  withFrozenCallStack do
    case preview p s of
      Just a -> pure a
      Nothing -> do
        footnote ( show context )
        failure s

throws
  :: ( Eq e, Exception e, HasCallStack, MonadIO m, MonadTest m, Show a )
  => IO a
  -> e
  -> m ()
throws action expected =
  withFrozenCallStack do
    liftIO ( tryAny action ) >>= \case
      Left actual ->
        case fromException actual of
          Nothing ->
            failure actual
          Just actual' ->
            actual' === expected
      Right result ->
        failure result
