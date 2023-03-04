-- Hello World client
-- Connects REQ socket to tcp://localhost:5559
-- Sends "Hello" to server, expects "World" back

import Control.Exception (throwIO)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Foldable (for_)
import Data.List.NonEmpty (pattern (:|))
import Text.Printf (printf)
import Zmq qualified
import Zmq.Requester qualified

main :: IO ()
main =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to server
    requester <- zmq Zmq.Requester.open
    zmq (Zmq.connect requester "tcp://localhost:5559")

    for_ [(0 :: Int) .. 9] \requestNbr -> do
      zmq (Zmq.Requester.send requester ("Hello" :| []))
      string :| _ <- zmq (Zmq.Requester.receive requester)
      printf "Received reply %d [%s]\n" requestNbr (ByteString.Char8.unpack string)

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
