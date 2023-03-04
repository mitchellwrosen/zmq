-- Hello World worker
-- Connects REP socket to tcp://localhost:5560
-- Expects "Hello" from client, replies with "World"

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (forever)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List.NonEmpty (pattern (:|))
import Text.Printf (printf)
import Zmq qualified
import Zmq.Replier qualified

main :: IO ()
main =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to clients
    responder <- zmq Zmq.Replier.open
    zmq (Zmq.connect responder "tcp://localhost:5560")

    forever do
      -- Wait for next request from client
      string :| _ <- zmq (Zmq.Replier.receive responder)
      printf "Received request: [%s]\n" (ByteString.Char8.unpack string)

      -- Do some 'work'
      threadDelay 1_000_000

      -- Send reply back to client
      zmq (Zmq.Replier.send responder ("World" :| []))

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
