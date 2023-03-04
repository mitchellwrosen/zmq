-- Reading from multiple sockets
-- This version uses zmq_poll()

import Control.Exception (throwIO)
import Control.Monad (forever)
import Zmq qualified
import Zmq.Puller qualified
import Zmq.Subscriber qualified

main :: IO ()
main =
  Zmq.run Zmq.defaultOptions do
    -- Connect to task ventilator
    receiver <- zmq Zmq.Puller.open
    zmq (Zmq.connect receiver "tcp://localhost:5557")

    -- Connect to weather server
    subscriber <- zmq Zmq.Subscriber.open
    zmq (Zmq.connect subscriber "tcp://localhost:5556")
    zmq (Zmq.Subscriber.subscribe subscriber "10001 ")

    -- Process messages from both sockets
    forever do
      let items =
            [ Zmq.canReceive receiver False,
              Zmq.canReceive subscriber True
            ]
      zmq (Zmq.poll items) >>= \case
        False -> do
          Zmq.Puller.receive receiver >>= \case
            Left _ -> pure ()
            Right _ ->
              -- Process task
              pure ()
        True -> do
          Zmq.Subscriber.receive subscriber >>= \case
            Left _ -> pure ()
            Right _ ->
              -- Process weather update
              pure ()

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
