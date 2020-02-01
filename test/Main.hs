{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty((:|)))
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog hiding (failure, test)
import Say
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import UnliftIO.Concurrent
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Zmq
import qualified Zmq.Publisher as Pub
import qualified Zmq.Subscriber as Sub
import qualified Zmq.XPublisher as Xpub
import qualified Zmq.XSubscriber as Xsub

main :: IO ()
main =
  Zmq.main Zmq.defaultOptions ( defaultMain ( testGroup "tests" tests ) )

tests :: [ TestTree ]
tests =
  [ test "Publisher open" do
      void openPub

  , test "Subscriber open" do
      void openSub

  , test "Pubsub" do
      ( pub, sub ) <- openPubSub
      Sub.subscribe sub "a"
      Pub.send pub ( "b" :| [] )
      Pub.send pub ( "a" :| [] )
      message <- Sub.recv sub
      message === "a" :| []

  , test "XPublisher recv subscription" do
      endpoint <- Gen.sample genInproc
      xpub <- openXpub
      sub <- openSub
      bindXpub xpub endpoint
      connectSub sub endpoint
      Sub.subscribe sub "hi"
      message <- Xpub.recv xpub
      message === Zmq.Subscribe "hi"

  , test "XSubscriber send subscription" do
      endpoint <- Gen.sample genInproc
      pub <- openPub
      xsub <- openXsub
      bindPub pub endpoint
      connectXsub xsub endpoint
      Xsub.subscribe xsub ""
      Pub.send pub ( "hi" :| [] )
      message <- Xsub.recv xsub
      message === "hi" :| []
  ]


openPubSub :: ( MonadIO m, MonadTest m ) => m ( Zmq.Publisher, Zmq.Subscriber )
openPubSub = do
  endpoint <- Gen.sample genInproc
  pub <- openPub
  sub <- openSub
  bindPub pub endpoint
  connectSub sub endpoint
  pure ( pub, sub )

--------------------------------------------------------------------------------

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

genInproc :: Gen ( Zmq.Endpoint 'Zmq.TransportInproc )
genInproc = do
  name <- Gen.text (Range.linear 1 255) Gen.unicode
  case Zmq.inproc name of
    Nothing -> error ( "bad inproc generator: " ++ show name )
    Just endpoint -> pure endpoint

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
