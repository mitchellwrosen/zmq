{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- Weather update client
-- Connects SUB socket to tcp://localhost:5556
-- Collects weather updates and finds avg temp in zipcode

import Control.Exception (throwIO)
import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.NonEmpty (pattern (:|))
import System.Environment (getArgs)
import Text.Printf (printf)
import Zmq qualified
import Zmq.Subscriber qualified
import Prelude hiding (filter)

main :: IO ()
main =
  zmq do
    Zmq.run Zmq.defaultOptions do
      -- Socket to talk to server
      putStrLn "Collecting updates from weather server..."
      Zmq.Subscriber.with \subscriber -> do
        zmq (Zmq.Subscriber.connect subscriber (Zmq.Tcp "localhost:5556"))

        -- Subscribe to zipcode, default is NYC, 10001
        filter <-
          getArgs <&> \case
            [] -> "10001 "
            filter : _ -> filter
        zmq (Zmq.Subscriber.subscribe subscriber (ByteString.Char8.pack filter))

        -- Process 100 updates
        temps <-
          replicateM 100 do
            string :| _ <- zmq (Zmq.Subscriber.receive subscriber)
            let [_zipcode :: Int, temperature, _relhumidity] =
                  string
                    & ByteString.Char8.unpack
                    & words
                    & map read
            pure (realToFrac @Int @Double temperature)
        printf "Average temperature for zipcode '%s' was %dF\n" filter (floor (sum temps / 100) :: Int)

        pure (Right ())

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
