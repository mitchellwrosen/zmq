module Main (main) where

import Control.Exception (throwIO)
import Control.Monad
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO qualified as Text
import System.Environment
import System.Exit
import Text.Read (readMaybe)
import Zmq qualified
import Zmq.Publisher qualified as Pub
import Zmq.Subscriber qualified as Sub

main :: IO ()
main =
  untry do
    Zmq.withContext Zmq.defaultOptions \context ->
      getArgs >>= \case
        ["bind", "publisher", readMaybe @Int -> Just port] -> do
          Pub.with context \pub -> do
            untry (Pub.bind pub (Zmq.Tcp ("127.0.0.1:" <> Text.pack (show port))))
            forever do
              line <- Text.getLine
              Pub.send pub (encodeUtf8 line :| [])
        ["subscribe", readMaybe @Int -> Just port] -> do
          Sub.with context \sub -> do
            untry (Sub.connect sub (Zmq.Tcp ("127.0.0.1:" <> Text.pack (show port))))
            Sub.subscribe sub ""
            forever do
              message <- Sub.receive sub
              for_ message \frame ->
                Text.putStrLn (decodeUtf8 frame)
              Text.putStrLn ""
        _ ->
          exitFailure

untry :: IO (Either Zmq.Error a) -> IO a
untry action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
