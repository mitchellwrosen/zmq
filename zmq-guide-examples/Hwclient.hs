-- Hello World client

import Control.Exception (throwIO)
import Control.Monad (forM_)
import Data.List.NonEmpty (pattern (:|))
import Text.Printf (printf)
import Zmq qualified
import Zmq.Requester qualified

main :: IO ()
main =
  zmq do
    Zmq.run Zmq.defaultOptions do
      putStrLn "Connecting to hello world server..."
      requester <- zmq Zmq.Requester.open
      zmq (Zmq.Requester.connect requester (Zmq.Tcp "localhost:5555"))

      forM_ [(0 :: Int) .. 9] \requestNbr -> do
        printf "Sending Hello %d...\n" requestNbr
        zmq (Zmq.Requester.send requester ("Hello" :| []))
        _ <- zmq (Zmq.Requester.receive requester)
        printf "Received World %d\n" requestNbr

      pure (Right ())

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
