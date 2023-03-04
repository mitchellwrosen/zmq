-- Hello World client

import Control.Exception (throwIO)
import Data.Foldable (for_)
import Data.List.NonEmpty (pattern (:|))
import Text.Printf (printf)
import Zmq qualified
import Zmq.Requester qualified

main :: IO ()
main =
  Zmq.run Zmq.defaultOptions do
    putStrLn "Connecting to hello world server..."
    requester <- zmq Zmq.Requester.open
    zmq (Zmq.connect requester "tcp://localhost:5555")

    for_ [(0 :: Int) .. 9] \requestNbr -> do
      printf "Sending Hello %d...\n" requestNbr
      zmq (Zmq.Requester.send requester ("Hello" :| []))
      _ <- zmq (Zmq.Requester.receive requester)
      printf "Received World %d\n" requestNbr

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
