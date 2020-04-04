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
import qualified Zmq.XPublisher as XPub
import qualified Zmq.XSubscriber as XSub

main :: IO ()
main = do
  bracket
    ( Zmq.newContext Zmq.defaultOptions )
    Zmq.terminateContext
    ( \ctx -> defaultMain ( testGroup "tests" ( tests ctx ) ) )

tests :: Zmq.Context -> [ TestTree ]
tests ctx =
  [ testGroup
      "Basic socket tests"
      ( basicTests
          [ somePublisherSocket
          , someSubscriberSocket
          , someXPublisherSocket
          , someXSubscriberSocket
          ]
          ctx
      )
  , testGroup "Publisher tests" ( publisherTests ctx )

  , test "Pubsub" do
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

      sub <- liftIO ( Sub.open ctx )
      liftIO ( Sub.connect sub endpoint )
      liftIO ( Sub.subscribe sub "" )
      putMVar readyVar ()
      liftIO ( replicateM_ 20000 ( Sub.recv sub ) )
      liftIO ( Pub.close pub )
      liftIO ( Sub.close sub )

  , test "XPublisher recv subscription" do
      endpoint <- randomInproc
      xpub <- liftIO ( XPub.open ctx )
      sub <- liftIO ( Sub.open ctx )
      liftIO ( XPub.bind xpub endpoint )
      liftIO ( Sub.connect sub endpoint )
      liftIO ( Sub.subscribe sub "hi" )
      message <- liftIO ( XPub.recv xpub )
      message === Zmq.Subscribe "hi"
      liftIO ( XPub.close xpub )
      liftIO ( Sub.close sub )

  , test "XSubscriber send subscription" do
      endpoint <- randomInproc
      pub <- liftIO ( Pub.open ctx )
      xsub <- liftIO ( XSub.open ctx )
      liftIO ( Pub.bind pub endpoint )
      liftIO ( XSub.connect xsub endpoint )
      liftIO ( XSub.subscribe xsub "" )
      liftIO ( Pub.send pub ( "hi" :| [] ) )
      message <- liftIO ( XSub.recv xsub )
      message === "hi" :| []
      liftIO ( Pub.close pub )
      liftIO ( XSub.close xsub )
  ]

basicTests :: [ SomeSocket ] -> Zmq.Context -> [ TestTree ]
basicTests sockets ctx =
  [ testGroup
      "open-close"
      ( map
          ( \SomeSocket{name, open, close} ->
              test name do
                sock <- open ctx
                close sock
          )
          sockets
      )

  , testGroup
      "open-close-close"
      ( map
          ( \SomeSocket{name, open, close} ->
              test name do
                sock <- open ctx
                close sock
                close sock
          )
          sockets
      )

  , testGroup
      "bind"
      ( map
          ( \SomeSocket{name, open, close, bind} ->
              test name do
                sock <- open ctx
                endpoint <- randomInproc
                bind sock endpoint
                close sock
          )
          sockets
      )

  , testGroup
      "bind-unbind"
      ( map
          ( \SomeSocket{name, open, close, bind, unbind} ->
              test name do
                sock <- open ctx
                endpoint <- randomInproc
                bind sock endpoint
                unbind sock endpoint
                close sock
          )
          sockets
      )

  , testGroup
      "unbind"
      ( map
          ( \SomeSocket{name, open, close, unbind} ->
              test name do
                sock <- open ctx
                endpoint <- randomInproc
                unbind sock endpoint
                close sock
          )
          sockets
      )

  , testGroup
      "unbind bogus"
      ( map
          ( \SomeSocket{name, open, close, unbind} ->
              test name do
                sock <- open ctx
                unbind sock ( Zmq.Tcp "" ) `throws`
                  Zmq.Error "zmq_unbind" 22 "Invalid argument"
                close sock
          )
          sockets
      )

  , testGroup
      "connect"
      ( map
          ( \SomeSocket{name, open, close, connect} ->
              test name do
                sock <- open ctx
                endpoint <- randomInproc
                connect sock endpoint
                close sock
          )
          sockets
      )

  , testGroup
      "connect-disconnect"
      ( map
          ( \SomeSocket{name, open, close, connect, disconnect} ->
              test name do
                sock <- open ctx
                endpoint <- randomInproc
                connect sock endpoint
                disconnect sock endpoint
                close sock
          )
          sockets
      )

  , testGroup
      "disconnect"
      ( map
          ( \SomeSocket{name, open, close, disconnect} ->
              test name do
                sock <- open ctx
                endpoint <- randomInproc
                disconnect sock endpoint
                close sock
          )
          sockets
      )

  , testGroup
      "disconnect bogus"
      ( map
          ( \SomeSocket{name, open, close, disconnect} ->
              test name do
                sock <- open ctx
                disconnect sock ( Zmq.Tcp "" ) `throws`
                  Zmq.Error "zmq_disconnect" 22 "Invalid argument"
                close sock
          )
          sockets
      )
  ]

publisherTests :: Zmq.Context -> [ TestTree ]
publisherTests ctx =
  [ test "send" do
      pub <- liftIO ( Pub.open ctx )
      liftIO ( Pub.send pub ( "" :| [] ) )
      liftIO ( Pub.close pub )
  ]

data SomeSocket
  = forall socket.
    SomeSocket
  { name :: TestName
  , open :: forall m. MonadIO m => Zmq.Context -> m socket
  , close :: forall m. MonadIO m => socket -> m ()
  , bind :: forall m transport. MonadIO m => socket -> Zmq.Endpoint transport -> m ()
  , unbind :: forall m transport. MonadIO m => socket -> Zmq.Endpoint transport -> m ()
  , connect :: forall m transport. MonadIO m => socket -> Zmq.Endpoint transport -> m ()
  , disconnect :: forall m transport. MonadIO m => socket -> Zmq.Endpoint transport -> m ()
  }

somePublisherSocket :: SomeSocket
somePublisherSocket =
  SomeSocket
    { name = "Publisher"
    , open = \ctx -> liftIO ( Pub.open ctx )
    , close = \sock -> liftIO ( Pub.close sock )
    , bind = \sock endpoint -> liftIO ( Pub.bind sock endpoint )
    , unbind = \sock endpoint -> liftIO ( Pub.unbind sock endpoint )
    , connect = \sock endpoint -> liftIO ( Pub.connect sock endpoint )
    , disconnect = \sock endpoint -> liftIO ( Pub.disconnect sock endpoint )
    }

someSubscriberSocket :: SomeSocket
someSubscriberSocket =
  SomeSocket
    { name = "Subscriber"
    , open = \ctx -> liftIO ( Sub.open ctx )
    , close = \sock -> liftIO ( Sub.close sock )
    , bind = \sock endpoint -> liftIO ( Sub.bind sock endpoint )
    , unbind = \sock endpoint -> liftIO ( Sub.unbind sock endpoint )
    , connect = \sock endpoint -> liftIO ( Sub.connect sock endpoint )
    , disconnect = \sock endpoint -> liftIO ( Sub.disconnect sock endpoint )
    }

someXPublisherSocket :: SomeSocket
someXPublisherSocket =
  SomeSocket
    { name = "XPublisher"
    , open = \ctx -> liftIO ( XPub.open ctx )
    , close = \sock -> liftIO ( XPub.close sock )
    , bind = \sock endpoint -> liftIO ( XPub.bind sock endpoint )
    , unbind = \sock endpoint -> liftIO ( XPub.unbind sock endpoint )
    , connect = \sock endpoint -> liftIO ( XPub.connect sock endpoint )
    , disconnect = \sock endpoint -> liftIO ( XPub.disconnect sock endpoint )
    }

someXSubscriberSocket :: SomeSocket
someXSubscriberSocket =
  SomeSocket
    { name = "XSubscriber"
    , open = \ctx -> liftIO ( XSub.open ctx )
    , close = \sock -> liftIO ( XSub.close sock )
    , bind = \sock endpoint -> liftIO ( XSub.bind sock endpoint )
    , unbind = \sock endpoint -> liftIO ( XSub.unbind sock endpoint )
    , connect = \sock endpoint -> liftIO ( XSub.connect sock endpoint )
    , disconnect = \sock endpoint -> liftIO ( XSub.disconnect sock endpoint )
    }

--------------------------------------------------------------------------------

openPubSub
  :: ( MonadIO m, MonadTest m )
  => Zmq.Context
  -> m ( Zmq.Publisher, Zmq.Subscriber )
openPubSub ctx = do
  endpoint <- randomInproc
  pub <- liftIO ( Pub.open ctx )
  sub <- liftIO ( Sub.open ctx )
  liftIO ( Pub.bind pub endpoint )
  liftIO ( Sub.connect sub endpoint )
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
