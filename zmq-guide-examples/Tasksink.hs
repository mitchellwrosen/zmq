-- Task sink
-- Binds PULL socket to tcp://localhost:5558
-- Collects results from workers via that socket

import Control.Exception (throwIO)
import Data.Foldable (for_)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Zmq qualified
import Zmq.Puller qualified

main :: IO ()
main =
  Zmq.run Zmq.defaultOptions do
    -- Prepare our socket
    receiver <- zmq Zmq.Puller.open
    zmq (Zmq.bind receiver "tcp//*:5558")

    -- Wait for start of batch
    _ <- zmq (Zmq.Puller.receive receiver)

    -- Start our clock now
    startTime <- getMonotonicTimeNSec

    -- Process 100 confirmations
    for_ [(0 :: Int) .. 99] \taskNbr -> do
      _ <- zmq (Zmq.Puller.receive receiver)
      putChar (if mod taskNbr 10 == 0 then ':' else '.')
      hFlush stdout
    -- Calculate and report duration of batch
    stopTime <- getMonotonicTimeNSec
    printf "Total elapsed time: %d msec\n" ((stopTime - startTime) `div` 1_000_000)

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
