-- Simple request-reply broker

import Control.Exception (throwIO)
import Control.Monad (forever, when)
import Zmq qualified
import Zmq.Dealer qualified
import Zmq.Router qualified

main :: IO ()
main =
  Zmq.run Zmq.defaultOptions do
    -- Prepare our sockets
    frontend <- zmq Zmq.Router.open
    backend <- zmq Zmq.Dealer.open
    zmq (Zmq.bind frontend "tcp://*:5559")
    zmq (Zmq.bind backend "tcp://*:5560")

    -- Initialize poll set
    let items =
          [ Zmq.canReceive frontend [False],
            Zmq.canReceive backend [True]
          ]
    -- Switch messages between sockets
    forever do
      results <- zmq (Zmq.poll items)
      when (elem False results) do
        message <- zmq (Zmq.Router.receive frontend)
        zmq (Zmq.Dealer.send backend message)
      when (elem True results) do
        message <- zmq (Zmq.Dealer.receive backend)
        zmq (Zmq.Router.send frontend message)

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
