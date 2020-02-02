{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor (void)
-- import Data.List (sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog hiding (failure, test)
-- import Say
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import UnliftIO.Concurrent
-- import UnliftIO.Async
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
main =
  Zmq.main Zmq.defaultOptions do
    defaultMain ( testGroup "tests" tests )

tests :: [ TestTree ]
tests =
  [ testGroup "Publisher tests" publisherTests

  , test "Subscriber open" do
      void openSub

  , test "Pubsub" do
      ( pub, sub ) <- openPubSub
      Sub.subscribe sub "a"
      Pub.send pub ( "b" :| [] )
      let message = "a" :| []
      Pub.send pub message
      Sub.recv sub >>= ( === message )

    -- Publish M messages to a socket from each of N threads. Ensure that the
    -- subscriber receives N*M.
  , test "Concurrent publisher" do
      subscribedVar <- newEmptyMVar

      endpoint <- randomInproc

      pub <- openCpub
      bindCpub pub endpoint

      let n = 10
      let m = 200
      replicateM_ n do
        ( liftIO . forkOS ) do
          readMVar subscribedVar
          replicateM_ m ( Cpub.send pub ( "" :| [] ) )

      sub <- openSub
      connectSub sub endpoint
      Sub.subscribe sub ""
      threadDelay 1_000_000 -- annoying...
      putMVar subscribedVar ()
      matches _Just () =<<
        liftIO ( timeout 5_000_000 ( replicateM_ ( n * m ) ( Sub.recv sub ) ) )
      Cpub.close pub

  , test "XPublisher recv subscription" do
      endpoint <- randomInproc
      xpub <- openXpub
      sub <- openSub
      bindXpub xpub endpoint
      connectSub sub endpoint
      Sub.subscribe sub "hi"
      message <- Xpub.recv xpub
      message === Zmq.Subscribe "hi"

  , test "XSubscriber send subscription" do
      endpoint <- randomInproc
      pub <- openPub
      xsub <- openXsub
      bindPub pub endpoint
      connectXsub xsub endpoint
      Xsub.subscribe xsub ""
      Pub.send pub ( "hi" :| [] )
      message <- Xsub.recv xsub
      message === "hi" :| []
  ]

publisherTests :: [ TestTree ]
publisherTests =
  [ test "Publisher open" do
      void openPub

  , test "Publisher close" do
      pub <- openPub
      Pub.close pub

  , test "Publisher bind" do
      pub <- openPub
      endpoint <- randomInproc
      bindPub pub endpoint

  , test "Publisher unbind" do
      pub <- openPub
      endpoint <- randomInproc
      bindPub pub endpoint
      Pub.unbind pub endpoint

  , test "Publisher unbind when not bound" do
      pub <- openPub
      endpoint <- randomInproc
      Pub.unbind pub endpoint

  , test "Publisher unbind bogus" do
      pub <- openPub
      Pub.unbind pub ( Zmq.Tcp "" )

  , test "Publisher connect" do
      pub <- openPub
      endpoint <- randomInproc
      connectPub pub endpoint

  , test "Publisher disconnect" do
      pub <- openPub
      endpoint <- randomInproc
      connectPub pub endpoint
      Pub.disconnect pub endpoint

  , test "Publisher disconnect when not connected" do
      pub <- openPub
      endpoint <- randomInproc
      Pub.disconnect pub endpoint

  , test "Publisher disconnect bogus" do
      pub <- openPub
      Pub.disconnect pub ( Zmq.Tcp "" )

  , test "Publisher send" do
      pub <- openPub
      Pub.send pub ( "" :| [] )
  ]

--------------------------------------------------------------------------------

openPubSub
  :: ( MonadIO m, MonadTest m )
  => m ( Zmq.Publisher, Zmq.Subscriber )
openPubSub = do
  endpoint <- randomInproc
  pub <- openPub
  sub <- openSub
  bindPub pub endpoint
  connectSub sub endpoint
  pure ( pub, sub )

--------------------------------------------------------------------------------

openCpub :: ( MonadIO m, MonadTest m ) => m Zmq.ConcurrentPublisher
openCpub =
  matches _Just () =<< Cpub.open

openPub :: ( MonadIO m, MonadTest m ) => m Zmq.Publisher
openPub =
  matches _Just () =<< Pub.open

openSub :: ( MonadIO m, MonadTest m ) => m Zmq.Subscriber
openSub =
  matches _Just () =<< Sub.open

openXpub :: ( MonadIO m, MonadTest m ) => m Zmq.XPublisher
openXpub =
  matches _Just () =<< Xpub.open

openXsub :: ( MonadIO m, MonadTest m ) => m Zmq.XSubscriber
openXsub =
  matches _Just () =<< Xsub.open

bindCpub
  :: ( MonadIO m, MonadTest m )
  => Zmq.ConcurrentPublisher
  -> Zmq.Endpoint transport
  -> m ()
bindCpub pub endpoint =
  matches _Right endpoint =<< Cpub.bind pub endpoint

bindPub
  :: ( MonadIO m, MonadTest m )
  => Zmq.Publisher
  -> Zmq.Endpoint transport
  -> m ()
bindPub pub endpoint =
  matches _Right endpoint =<< Pub.bind pub endpoint

bindXpub
  :: ( MonadIO m, MonadTest m )
  => Zmq.XPublisher
  -> Zmq.Endpoint transport
  -> m ()
bindXpub pub endpoint =
  matches _Right endpoint =<< Xpub.bind pub endpoint

connectPub
  :: ( MonadIO m, MonadTest m )
  => Zmq.Publisher
  -> Zmq.Endpoint transport
  -> m ()
connectPub pub endpoint =
  matches _Right endpoint =<< Pub.connect pub endpoint

connectSub
  :: ( MonadIO m, MonadTest m )
  => Zmq.Subscriber
  -> Zmq.Endpoint transport
  -> m ()
connectSub sub endpoint =
  matches _Right endpoint =<< Sub.connect sub endpoint

connectXsub
  :: ( MonadIO m, MonadTest m )
  => Zmq.XSubscriber
  -> Zmq.Endpoint transport
  -> m ()
connectXsub sub endpoint =
  matches _Right endpoint =<< Xsub.connect sub endpoint


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
