{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (forever, replicateM, replicateM_, when)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List.NonEmpty
import GHC.Clock (getMonotonicTimeNSec)
import Ki qualified
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
    ["taskwork2"] -> taskwork2
    ["tasksink2"] -> tasksink2
    ["mtserver"] -> mtserver
    _ -> pure ()

-- Hello World server
hwserver :: IO ()
hwserver =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to clients
    responder <- unwrap Zmq.Replier.open
    unwrap (Zmq.bind responder "tcp://*:5555")

    forever do
      _ <- unwrap (Zmq.Replier.receive responder)
      putStrLn "Received Hello"
      threadDelay 1_000_000 -- Do some work
      unwrap (Zmq.Replier.send responder "World")

-- Hello World client
hwclient :: IO ()
hwclient =
  Zmq.run Zmq.defaultOptions do
    putStrLn "Connecting to hello world server..."
    requester <- unwrap Zmq.Requester.open
    unwrap (Zmq.connect requester "tcp://localhost:5555")

    for_ [(0 :: Int) .. 9] \requestNbr -> do
      printf "Sending Hello %d...\n" requestNbr
      unwrap (Zmq.Requester.send requester "Hello")
      _ <- unwrap (Zmq.Requester.receive requester)
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
    publisher <- unwrap (Zmq.Publisher.open Zmq.Publisher.defaultOptions)
    unwrap (Zmq.bind publisher "tcp://*:5556")

    forever do
      -- Get values that will fool the boss
      zipcode <- uniformRM (0 :: Int, 99999) globalStdGen
      temperature <- uniformRM (-80 :: Int, 134) globalStdGen
      relhumidity <- uniformRM (10 :: Int, 59) globalStdGen

      -- Send message to all subscribers
      let update = ByteString.Char8.pack (printf "%05d %d %d" zipcode temperature relhumidity)
      unwrap (Zmq.Publisher.send publisher update "")

-- Weather update client
-- Connects SUB socket to tcp://localhost:5556
-- Collects weather updates and finds avg temp in zipcode
wuclient :: IO ()
wuclient =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to server
    putStrLn "Collecting updates from weather server..."
    subscriber <- unwrap Zmq.Subscriber.open
    unwrap (Zmq.connect subscriber "tcp://localhost:5556")

    -- Subscribe to zipcode, default is NYC, 10001
    filter <-
      getArgs <&> \case
        [] -> "10001 "
        filter : _ -> filter
    unwrap (Zmq.Subscriber.subscribe subscriber (ByteString.Char8.pack filter))

    -- Process 100 updates
    temps <-
      replicateM 100 do
        (string, _) <- unwrap (Zmq.Subscriber.receive subscriber)
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
    sender <- unwrap Zmq.Pusher.open
    unwrap (Zmq.bind sender "tcp://*:5557")

    -- Socket to send start of batch message on
    sink <- unwrap Zmq.Pusher.open
    unwrap (Zmq.connect sink "tcp://localhost:5558")

    putStrLn "Press Enter when the workers are ready"
    _ <- getLine
    putStrLn "Sending tasks to workers..."

    -- The first message is "0" and signals start of batch
    unwrap (Zmq.Pusher.send sink "0")

    -- Send 100 tasks
    workloads <-
      replicateM 100 do
        -- Random workload from 1 to 100msecs
        workload <- uniformRM (1 :: Int, 100) globalStdGen
        unwrap (Zmq.Pusher.send sender (ByteString.Char8.pack (printf "%d" workload)))
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
    receiver <- unwrap Zmq.Puller.open
    unwrap (Zmq.connect receiver "tcp://localhost:5557")

    -- Socket to send messages to
    sender <- unwrap Zmq.Pusher.open
    unwrap (Zmq.connect sender "tcp://localhost:5558")

    -- Process tasks forever
    forever do
      string <- unwrap (Zmq.Puller.receive receiver)
      printf "%s." (ByteString.Char8.unpack string) -- Show progress
      hFlush stdout
      threadDelay (read (ByteString.Char8.unpack string) * 1_000) -- Do the work
      unwrap (Zmq.Pusher.send sender "")

-- Task sink
-- Binds PULL socket to tcp://localhost:5558
-- Collects results from workers via that socket
tasksink :: IO ()
tasksink =
  Zmq.run Zmq.defaultOptions do
    -- Prepare our socket
    receiver <- unwrap Zmq.Puller.open
    unwrap (Zmq.bind receiver "tcp//*:5558")

    -- Wait for start of batch
    _ <- unwrap (Zmq.Puller.receive receiver)

    -- Start our clock now
    startTime <- getMonotonicTimeNSec

    -- Process 100 confirmations
    for_ [(0 :: Int) .. 99] \taskNbr -> do
      _ <- unwrap (Zmq.Puller.receive receiver)
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
    receiver <- unwrap Zmq.Puller.open
    unwrap (Zmq.connect receiver "tcp://localhost:5557")

    -- Connect to weather server
    subscriber <- unwrap Zmq.Subscriber.open
    unwrap (Zmq.connect subscriber "tcp://localhost:5556")
    unwrap (Zmq.Subscriber.subscribe subscriber "10001 ")

    -- Process messages from both sockets
    forever do
      let items =
            [ Zmq.canReceive receiver [False],
              Zmq.canReceive subscriber [True]
            ]
      results <- unwrap (Zmq.poll items)
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
    requester <- unwrap Zmq.Requester.open
    unwrap (Zmq.connect requester "tcp://localhost:5559")

    for_ [(0 :: Int) .. 9] \requestNbr -> do
      unwrap (Zmq.Requester.send requester "Hello")
      string <- unwrap (Zmq.Requester.receive requester)
      printf "Received reply %d [%s]\n" requestNbr (ByteString.Char8.unpack string)

-- Hello World worker
-- Connects REP socket to tcp://localhost:5560
-- Expects "Hello" from client, replies with "World"
rrworker :: IO ()
rrworker =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to clients
    responder <- unwrap Zmq.Replier.open
    unwrap (Zmq.connect responder "tcp://localhost:5560")

    forever do
      -- Wait for next request from client
      string <- unwrap (Zmq.Replier.receive responder)
      printf "Received request: [%s]\n" (ByteString.Char8.unpack string)

      -- Do some 'work'
      threadDelay 1_000_000

      -- Send reply back to client
      unwrap (Zmq.Replier.send responder "World")

-- Simple request-reply broker
rrbroker :: IO ()
rrbroker =
  Zmq.run Zmq.defaultOptions do
    -- Prepare our sockets
    frontend <- unwrap Zmq.Router.open
    backend <- unwrap Zmq.Dealer.open
    unwrap (Zmq.bind frontend "tcp://*:5559")
    unwrap (Zmq.bind backend "tcp://*:5560")

    -- Initialize poll set
    let items =
          [ Zmq.canReceive frontend [False],
            Zmq.canReceive backend [True]
          ]
    -- Switch messages between sockets
    forever do
      results <- unwrap (Zmq.poll items)
      when (elem False results) do
        (identity, message) <- unwrap (Zmq.Router.receive frontend)
        unwrap (Zmq.Dealer.sends backend (identity :| [message]))
      when (elem True results) do
        identity :| message : _ <- unwrap (Zmq.Dealer.receives backend)
        unwrap (Zmq.Router.send frontend identity message)

-- Weather proxy device
wuproxy :: IO ()
wuproxy =
  Zmq.run Zmq.defaultOptions do
    -- This is where the weather server sits
    frontend <- unwrap Zmq.XSubscriber.open
    unwrap (Zmq.connect frontend "tcp://192.168.55.210:5556")

    -- This is our public endpoint for subscribers
    backend <- unwrap (Zmq.XPublisher.open Zmq.Publisher.defaultOptions)
    unwrap (Zmq.bind backend "tcp://10.1.1.0:8100")

    -- Run the proxy until the user interrupts us
    let items =
          [ Zmq.canReceive frontend [False],
            Zmq.canReceive backend [True]
          ]
    forever do
      results <- unwrap (Zmq.poll items)
      when (elem False results) do
        (topic, message) <- unwrap (Zmq.XSubscriber.receive frontend)
        unwrap (Zmq.XPublisher.send backend topic message)
      when (elem True results) do
        message <- unwrap (Zmq.XPublisher.receive backend)
        unwrap (Zmq.XSubscriber.send frontend message)

-- Task worker - design 2
-- Adds pub-sub flow to receive and respond to kill signal
taskwork2 :: IO ()
taskwork2 =
  Zmq.run Zmq.defaultOptions do
    -- Socket to receive messages on
    receiver <- unwrap Zmq.Puller.open
    unwrap (Zmq.connect receiver "tcp://localhost:5557")

    -- Socket to send messages to
    sender <- unwrap Zmq.Pusher.open
    unwrap (Zmq.connect sender "tcp://localhost:5558")

    -- Socket for control input
    controller <- unwrap Zmq.Subscriber.open
    unwrap (Zmq.connect controller "tcp://localhost:5559")
    unwrap (Zmq.Subscriber.subscribe controller "")

    -- Process messages from either socket
    let loop = do
          let items =
                [ Zmq.canReceive receiver [False],
                  Zmq.canReceive controller [True]
                ]
          results <- unwrap (Zmq.poll items)
          when (elem False results) do
            string <- unwrap (Zmq.Puller.receive receiver)
            printf "%s." (ByteString.Char8.unpack string) -- Show progress
            hFlush stdout
            threadDelay (read (ByteString.Char8.unpack string) * 1_000) -- Do the work
            unwrap (Zmq.Pusher.send sender "")
          -- Any waiting controller command acts as 'KILL'
          when (not (elem True results)) do
            loop
    loop

-- Task sink - design 2
-- Adds pub-sub flow to send kill signal to workers
tasksink2 :: IO ()
tasksink2 =
  Zmq.run Zmq.defaultOptions do
    -- Socket to receive messages on
    receiver <- unwrap Zmq.Puller.open
    unwrap (Zmq.bind receiver "tcp://*:5558")

    -- Socket for worker control
    controller <- unwrap (Zmq.Publisher.open Zmq.Publisher.defaultOptions)
    unwrap (Zmq.bind controller "tcp://*:5559")

    -- Wait for start of batch
    _ <- unwrap (Zmq.Puller.receive receiver)

    -- Start our clock now
    startTime <- getMonotonicTimeNSec

    -- Process 100 confirmations
    for_ [(0 :: Int) .. 99] \taskNbr -> do
      _ <- unwrap (Zmq.Puller.receive receiver)
      putChar (if mod taskNbr 10 == 0 then ':' else '.')
      hFlush stdout
    stopTime <- getMonotonicTimeNSec
    printf "Total elapsed time: %d msec\n" ((stopTime - startTime) `div` 1_000_000)

    -- Send kill signal to workers
    unwrap (Zmq.Publisher.send controller "KILL" "")

-- Multithreaded Hello World server
mtserver :: IO ()
mtserver =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to clients
    clients <- unwrap Zmq.Router.open
    unwrap (Zmq.bind clients "tcp://*:5555")

    -- Socket to talk to workers
    workers <- unwrap Zmq.Dealer.open
    unwrap (Zmq.bind workers "inproc://workers")

    -- Launch pool of worker threads
    Ki.scoped \scope -> do
      replicateM_ 5 do
        Ki.fork_ scope do
          -- Socket to talk to dispatcher
          receiver <- unwrap Zmq.Replier.open
          unwrap (Zmq.connect receiver "inproc://workers")

          forever do
            string <- unwrap (Zmq.Replier.receive receiver)
            printf "Received request: [%s]\n" (ByteString.Char8.unpack string)
            -- Do some 'work'
            threadDelay 1_000_000
            -- Send reply back to client
            unwrap (Zmq.Replier.send receiver "World")
      -- Connect work threads to client threads via a queue proxy
      let items =
            [ Zmq.canReceive clients [False],
              Zmq.canReceive workers [True]
            ]
      forever do
        results <- unwrap (Zmq.poll items)
        when (elem False results) do
          (identity, message) <- unwrap (Zmq.Router.receives clients)
          unwrap (Zmq.Dealer.sends workers (List.NonEmpty.cons identity message))
        when (elem True results) do
          identity :| frame : frames <- unwrap (Zmq.Dealer.receives workers)
          unwrap (Zmq.Router.sends clients identity (frame :| frames))

------------------------------------------------------------------------------------------------------------------------
-- Utils

unwrap :: IO (Either Zmq.Error a) -> IO a
unwrap action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
