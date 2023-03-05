{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (forever, replicateM, when)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List.NonEmpty (pattern (:|))
import GHC.Clock (getMonotonicTimeNSec)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Random.Stateful (globalStdGen, uniformRM)
import Text.Printf (printf)
import Zmq qualified
import Zmq.Dealer qualified
import Zmq.Publisher qualified
import Zmq.Puller qualified
import Zmq.Pusher qualified
import Zmq.Replier qualified
import Zmq.Requester qualified
import Zmq.Router qualified
import Zmq.Subscriber qualified
import Zmq.XPublisher qualified
import Zmq.XSubscriber qualified
import Prelude hiding (filter)

main :: IO ()
main =
  getArgs >>= \case
    ["hwserver"] -> hwserver
    ["hwclient"] -> hwclient
    ["version"] -> version
    ["wuserver"] -> wuserver
    ["wuclient"] -> wuclient
    ["taskvent"] -> taskvent
    ["taskwork"] -> taskwork
    ["tasksink"] -> tasksink
    ["mspoller"] -> mspoller
    ["rrclient"] -> rrclient
    ["rrworker"] -> rrworker
    ["rrbroker"] -> rrbroker
    ["wuproxy"] -> wuproxy
    _ -> pure ()

-- Hello World server
hwserver :: IO ()
hwserver =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to clients
    responder <- zmq Zmq.Replier.open
    zmq (Zmq.bind responder "tcp://*:5555")

    forever do
      _ <- zmq (Zmq.Replier.receive responder)
      putStrLn "Received Hello"
      threadDelay 1_000_000 -- Do some work
      zmq (Zmq.Replier.send responder ("World" :| []))

-- Hello World client
hwclient :: IO ()
hwclient =
  Zmq.run Zmq.defaultOptions do
    putStrLn "Connecting to hello world server..."
    requester <- zmq Zmq.Requester.open
    zmq (Zmq.connect requester "tcp://localhost:5555")

    for_ [(0 :: Int) .. 9] \requestNbr -> do
      printf "Sending Hello %d...\n" requestNbr
      zmq (Zmq.Requester.send requester ("Hello" :| []))
      _ <- zmq (Zmq.Requester.receive requester)
      printf "Received World %d\n" requestNbr

-- Report 0MQ version
version :: IO ()
version = do
  let (major, minor, patch) = Zmq.version
  printf "Current 0MQ version is %d.%d.%d\n" major minor patch

-- Weather update server
-- Binds PUB socket to tcp://*:5556
-- Publishes random weather updates
wuserver :: IO ()
wuserver =
  Zmq.run Zmq.defaultOptions do
    -- Prepare our publisher
    publisher <- zmq Zmq.Publisher.open
    zmq (Zmq.bind publisher "tcp://*:5556")

    forever do
      -- Get values that will fool the boss
      zipcode <- uniformRM (0 :: Int, 99999) globalStdGen
      temperature <- uniformRM (-80 :: Int, 134) globalStdGen
      relhumidity <- uniformRM (10 :: Int, 59) globalStdGen

      -- Send message to all subscribers
      let update = ByteString.Char8.pack (printf "%05d %d %d" zipcode temperature relhumidity)
      zmq (Zmq.Publisher.send publisher update [])

-- Weather update client
-- Connects SUB socket to tcp://localhost:5556
-- Collects weather updates and finds avg temp in zipcode
wuclient :: IO ()
wuclient =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to server
    putStrLn "Collecting updates from weather server..."
    subscriber <- zmq Zmq.Subscriber.open
    zmq (Zmq.connect subscriber "tcp://localhost:5556")

    -- Subscribe to zipcode, default is NYC, 10001
    filter <-
      getArgs <&> \case
        [] -> "10001 "
        filter : _ -> filter
    zmq (Zmq.Subscriber.subscribe subscriber (ByteString.Char8.pack filter))

    -- Process 100 updates
    temps <-
      replicateM 100 do
        string :| _ <- zmq (Zmq.Subscriber.receive subscriber)
        let [_zipcode :: Int, temperature, _relhumidity] = map read (words (ByteString.Char8.unpack string))
        pure (realToFrac @Int @Double temperature)
    printf "Average temperature for zipcode '%s' was %dF\n" filter (floor (sum temps / 100) :: Int)

-- Task ventilator
-- Binds PUSH socket to tcp://localhost:5557
-- Sends batch of tasks to workers via that socket
taskvent :: IO ()
taskvent =
  Zmq.run Zmq.defaultOptions do
    -- Socket to send messages on
    sender <- zmq Zmq.Pusher.open
    zmq (Zmq.bind sender "tcp://*:5557")

    -- Socket to send start of batch message on
    sink <- zmq Zmq.Pusher.open
    zmq (Zmq.connect sink "tcp://localhost:5558")

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

-- Task worker
-- Connects PULL socket to tcp://localhost:5557
-- Collects workloads from ventilator via that socket
-- Connects PUSH socket to tcp://localhost:5558
-- Sends results to sink via that socket
taskwork :: IO ()
taskwork =
  Zmq.run Zmq.defaultOptions do
    -- Socket to receive messages on
    receiver <- zmq Zmq.Puller.open
    zmq (Zmq.connect receiver "tcp://localhost:5557")

    -- Socket to send messages to
    sender <- zmq Zmq.Pusher.open
    zmq (Zmq.connect sender "tcp://localhost:5558")

    -- Process tasks forever
    forever do
      string :| _ <- zmq (Zmq.Puller.receive receiver)
      printf "%s." (ByteString.Char8.unpack string) -- Show progress
      hFlush stdout
      threadDelay (read (ByteString.Char8.unpack string) * 1_000) -- Do the work
      zmq (Zmq.Pusher.send sender ("" :| [])) -- Send results to sink

-- Task sink
-- Binds PULL socket to tcp://localhost:5558
-- Collects results from workers via that socket
tasksink :: IO ()
tasksink =
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

-- Reading from multiple sockets
-- This version uses zmq_poll()
mspoller :: IO ()
mspoller =
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
            [ Zmq.canReceive receiver [False],
              Zmq.canReceive subscriber [True]
            ]
      results <- zmq (Zmq.poll items)
      when (elem False results) do
        Zmq.Puller.receive receiver >>= \case
          Left _ -> pure ()
          Right _ ->
            -- Process task
            pure ()
      when (elem True results) do
        Zmq.Subscriber.receive subscriber >>= \case
          Left _ -> pure ()
          Right _ ->
            -- Process weather update
            pure ()

-- Hello World client
-- Connects REQ socket to tcp://localhost:5559
-- Sends "Hello" to server, expects "World" back
rrclient :: IO ()
rrclient =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to server
    requester <- zmq Zmq.Requester.open
    zmq (Zmq.connect requester "tcp://localhost:5559")

    for_ [(0 :: Int) .. 9] \requestNbr -> do
      zmq (Zmq.Requester.send requester ("Hello" :| []))
      string :| _ <- zmq (Zmq.Requester.receive requester)
      printf "Received reply %d [%s]\n" requestNbr (ByteString.Char8.unpack string)

-- Hello World worker
-- Connects REP socket to tcp://localhost:5560
-- Expects "Hello" from client, replies with "World"
rrworker :: IO ()
rrworker =
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

-- Simple request-reply broker
rrbroker :: IO ()
rrbroker =
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

-- Weather proxy device
wuproxy :: IO ()
wuproxy = do
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

------------------------------------------------------------------------------------------------------------------------
-- Utils

zmq :: IO (Either Zmq.Error a) -> IO a
zmq action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
