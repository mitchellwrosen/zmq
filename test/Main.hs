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
import UnliftIO.Timeout
import qualified Data.Text as Text
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Zmq
import qualified Zmq.ConcurrentPublisher as Cpub
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

  , test "Subscriber open" do
      sub <- Sub.open ctx
      Sub.close sub

  , test "Pubsub" do
      ( pub, sub ) <- openPubSub ctx
      Sub.subscribe sub "a"
      Pub.send pub ( "b" :| [] )
      let message = "a" :| []
      Pub.send pub message
      Sub.recv sub >>= ( === message )
      Pub.close pub
      Sub.close sub

    -- Publish M messages to a socket from each of N threads. Ensure that the
    -- subscriber receives N*M.
  , test "Concurrent publisher" do
      endpoint <- randomInproc

      pub <- Cpub.open ctx
      Cpub.bind pub endpoint

      replicateM_ 10 do
        ( liftIO . forkOS ) do
          forever do
            Cpub.send pub ( "" :| [] )

      sub <- Sub.open ctx
      Sub.connect sub endpoint
      Sub.subscribe sub ""
      matches _Just () =<<
        liftIO do
          timeout 5_000_000 do
            replicateM_ 2000 ( Sub.recv sub )
      Cpub.close pub
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
      pub <- Pub.open ctx
      xsub <- Xsub.open ctx
      Pub.bind pub endpoint
      Xsub.connect xsub endpoint
      Xsub.subscribe xsub ""
      Pub.send pub ( "hi" :| [] )
      message <- Xsub.recv xsub
      message === "hi" :| []
      Pub.close pub
      Xsub.close xsub
  ]

publisherTests :: Zmq.Context -> [ TestTree ]
publisherTests ctx =
  [ test "Publisher open close" do
      pub <- Pub.open ctx
      Pub.close pub

  , test "Publisher bind" do
      pub <- Pub.open ctx
      endpoint <- randomInproc
      Pub.bind pub endpoint
      Pub.close pub

  , test "Publisher unbind" do
      pub <- Pub.open ctx
      endpoint <- randomInproc
      Pub.bind pub endpoint
      Pub.unbind pub endpoint
      Pub.close pub

  , test "Publisher unbind when not bound" do
      pub <- Pub.open ctx
      endpoint <- randomInproc
      Pub.unbind pub endpoint
      Pub.close pub

  , test "Publisher unbind bogus" do
      pub <- Pub.open ctx
      Pub.unbind pub ( Zmq.Tcp "" ) `throws`
        Zmq.Exception "zmq_unbind" 22 "Invalid argument"
      Pub.close pub

  , test "Publisher connect" do
      pub <- Pub.open ctx
      endpoint <- randomInproc
      Pub.connect pub endpoint
      Pub.close pub

  , test "Publisher disconnect" do
      pub <- Pub.open ctx
      endpoint <- randomInproc
      Pub.connect pub endpoint
      Pub.disconnect pub endpoint
      Pub.close pub

  , test "Publisher disconnect when not connected" do
      pub <- Pub.open ctx
      endpoint <- randomInproc
      Pub.disconnect pub endpoint
      Pub.close pub

  , test "Publisher disconnect bogus" do
      pub <- Pub.open ctx
      Pub.disconnect pub ( Zmq.Tcp "" ) `throws`
        Zmq.Exception "zmq_disconnect" 22 "Invalid argument"
      Pub.close pub

  , test "Publisher send" do
      pub <- Pub.open ctx
      Pub.send pub ( "" :| [] )
      Pub.close pub
  ]

--------------------------------------------------------------------------------

openPubSub
  :: ( MonadIO m, MonadTest m )
  => Zmq.Context
  -> m ( Zmq.Publisher, Zmq.Subscriber )
openPubSub ctx = do
  endpoint <- randomInproc
  pub <- Pub.open ctx
  sub <- Sub.open ctx
  Pub.bind pub endpoint
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
