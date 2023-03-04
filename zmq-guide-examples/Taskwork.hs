-- Task worker
-- Connects PULL socket to tcp://localhost:5557
-- Collects workloads from ventilator via that socket
-- Connects PUSH socket to tcp://localhost:5558
-- Sends results to sink via that socket

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List.NonEmpty (pattern (:|))
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Zmq qualified
import Zmq.Puller qualified
import Zmq.Pusher qualified

main :: IO ()
main =
  Zmq.run Zmq.defaultOptions do
    -- Socket to receive messages on
    receiver <- zmq Zmq.Puller.open
    zmq (Zmq.Puller.connect receiver (Zmq.Tcp "localhost:5557"))

    -- Socket to send messages to
    sender <- zmq Zmq.Pusher.open
    zmq (Zmq.Pusher.connect sender (Zmq.Tcp "localhost:5558"))

    -- Process tasks forever
    forever do
      string :| _ <- zmq (Zmq.Puller.receive receiver)
      printf "%s." (ByteString.Char8.unpack string) -- Show progress
      hFlush stdout
      threadDelay (read (ByteString.Char8.unpack string) * 1_000) -- Do the work
      zmq (Zmq.Pusher.send sender ("" :| [])) -- Send results to sink

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
