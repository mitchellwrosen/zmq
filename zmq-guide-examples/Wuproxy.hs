-- Weather proxy device

import Control.Exception (throwIO)
import Control.Monad (forever, when)
import Data.List.NonEmpty (pattern (:|))
import Zmq qualified
import Zmq.XPublisher qualified
import Zmq.XSubscriber qualified

main :: IO ()
main = do
  -- This is where the weather server sits
  frontend <- zmq Zmq.XSubscriber.open
  zmq (Zmq.connect frontend "tcp://192.168.55.210:5556")

  -- This is our public endpoint for subscribers
  backend <- zmq Zmq.XPublisher.open
  zmq (Zmq.bind backend "tcp://10.1.1.0:8100")

  -- Run the proxy until the user interrupts us
  let items =
        [ Zmq.canReceive frontend [False],
          Zmq.canReceive backend [True]
        ]
  forever do
    results <- zmq (Zmq.poll items)
    when (elem False results) do
      topic :| message <- zmq (Zmq.XSubscriber.receive frontend)
      zmq (Zmq.XPublisher.send backend topic message)
    when (elem True results) do
      message <- zmq (Zmq.XPublisher.receive backend)
      zmq (Zmq.XSubscriber.send frontend message)

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
