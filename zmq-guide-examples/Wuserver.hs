-- Weather update server
-- Binds PUB socket to tcp://*:5556
-- Publishes random weather updates

import Control.Exception (throwIO)
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List.NonEmpty (pattern (:|))
import System.Random.Stateful (globalStdGen, uniformRM)
import Text.Printf (printf)
import Zmq qualified
import Zmq.Publisher qualified

main :: IO ()
main =
  zmq do
    Zmq.run Zmq.defaultOptions do
      -- Prepare our publisher
      Zmq.Publisher.with \publisher -> do
        zmq (Zmq.Publisher.bind publisher (Zmq.Tcp "*:5556"))

        forever do
          -- Get values that will fool the boss
          zipcode <- uniformRM (0 :: Int, 99999) globalStdGen
          temperature <- uniformRM (-80 :: Int, 134) globalStdGen
          relhumidity <- uniformRM (10 :: Int, 59) globalStdGen

          -- Send message to all subscribers
          let update = ByteString.Char8.pack (printf "%05d %d %d" zipcode temperature relhumidity)
          Zmq.Publisher.send publisher "" (update :| [])

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
