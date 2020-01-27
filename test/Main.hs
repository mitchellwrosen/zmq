module Main where

import Control.Lens
import Data.Foldable (for_)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty((:|)))
import EasyTest hiding (matches, unitTest)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import qualified EasyTest

import qualified Zmq

main :: IO Summary
main =
  ( Zmq.main options . run . tests )
    [ unitTest "socket.pub" do
        void ( matches _Just =<< Zmq.pubSocket )
    , unitTest "socket.sub" do
        void ( matches _Just =<< Zmq.pubSocket )
    , unitTest "socket.maxSockets" do
        s0 <- matches _Just =<< Zmq.pubSocket
        s1 <- matches _Just =<< Zmq.pubSocket
        () <- matches _Nothing =<< Zmq.pubSocket
        for_ [ s0, s1 ] Zmq.close -- keep sockets alive

    , unitTest "pubsub" do
        let endpoint = Zmq.Inproc "foo"
        pub <- matches _Just =<< Zmq.pubSocket
        sub <- matches _Just =<< Zmq.subSocket
        matches _Right =<< Zmq.bind pub endpoint
        matches _Right =<< Zmq.connect sub endpoint
        Zmq.subscribe sub ""
        matches _Right =<< Zmq.send pub "hi"
        Zmq.recv sub >>= \case
          "hi" :| [] -> pure ()
          message -> do
            annotate ( show message )
            failure
    ]

  where
    options :: Zmq.Options
    options =
      Zmq.defaultOptions
        { Zmq.maxSockets = 2
        }

unitTest
  :: HasCallStack
  => String
  -> PropertyT IO ()
  -> Test
unitTest name test =
  scope name ( EasyTest.unitTest test )

-- Easytests's matches, but returns the a.
matches
  :: ( HasCallStack, Show s )
  => Prism' s a
  -> s
  -> PropertyT IO a
matches p s =
  withFrozenCallStack
    ( case preview p s of
        Just a -> pure a
        Nothing -> do
          annotate ( show s )
          footnote "Prism failed to match"
          failure
    )
