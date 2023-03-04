-- Task ventilator
-- Binds PUSH socket to tcp://localhost:5557
-- Sends batch of tasks to workers via that socket

import Control.Exception (throwIO)
import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List.NonEmpty (pattern (:|))
import System.Random.Stateful (globalStdGen, uniformRM)
import Text.Printf (printf)
import Zmq qualified
import Zmq.Pusher qualified

main :: IO ()
main =
  Zmq.run Zmq.defaultOptions do
    -- Socket to send messages on
    sender <- zmq Zmq.Pusher.open
    zmq (Zmq.Pusher.bind sender (Zmq.Tcp "*:5557"))

    -- Socket to send start of batch message on
    sink <- zmq Zmq.Pusher.open
    zmq (Zmq.Pusher.connect sink (Zmq.Tcp "localhost:5558"))

    putStrLn "Press Enter when the workers are ready"
    _ <- getLine
    putStrLn "Sending tasks to workers..."

    -- The first message is "0" and signals start of batch
    zmq (Zmq.Pusher.send sink ("0" :| []))

    -- Send 100 tasks
    workloads <-
      replicateM 100 do
        -- Random workload from 1 to 100msecs
        workload <- uniformRM (1 :: Int, 100) globalStdGen
        zmq (Zmq.Pusher.send sender (ByteString.Char8.pack (printf "%d" workload) :| []))
        pure workload
    printf "Total expected cost: %d msec\n" (sum workloads)

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
