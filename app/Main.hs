module Main where

import Control.Monad
import Data.Foldable
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Environment
import System.Exit
import Text.Read (readMaybe)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import qualified Zmq
import qualified Zmq.Publisher as Pub
import qualified Zmq.Subscriber as Sub


main :: IO ()
main = do
  context <- Zmq.newContext Zmq.defaultOptions

  getArgs >>= \case
    [ "bind", "publisher", readMaybe @Int -> Just port ] -> do
      pub <- Pub.open context
      Pub.bind pub ( Zmq.Tcp ( "127.0.0.1:" <> Text.pack ( show port ) ) )
      forever do
        line <- Text.getLine
        Pub.send pub ( encodeUtf8 line :| [] )

    [ "subscribe", readMaybe @Int -> Just port ] -> do
      sub <- Sub.open context
      Sub.connect sub ( Zmq.Tcp ( "127.0.0.1:" <> Text.pack ( show port ) ) )
      Sub.subscribe sub ""
      forever do
        message <- Sub.recv sub
        for_ message \frame ->
          Text.putStrLn ( decodeUtf8 frame )
        Text.putStrLn ""

    _ ->
      exitFailure
