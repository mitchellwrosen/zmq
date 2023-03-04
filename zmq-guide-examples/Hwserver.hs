-- Hello World server

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (forever)
import Data.List.NonEmpty (pattern (:|))
import Zmq qualified
import Zmq.Replier qualified

main :: IO ()
main =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to clients
    responder <- zmq Zmq.Replier.open
    zmq (Zmq.bind responder "tcp://*:5555")

    forever do
      _ <- zmq (Zmq.Replier.receive responder)
      putStrLn "Received Hello"
      threadDelay 1_000_000 -- Do some work
      zmq (Zmq.Replier.send responder ("World" :| []))

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
