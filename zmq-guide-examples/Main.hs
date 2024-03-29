{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

import Control.Concurrent (threadDelay)
import Control.Exception (throwIO)
import Control.Monad (forever, replicateM, replicateM_, when)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Foldable (for_)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text qualified as Text
import Data.Void (Void)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (atomically)
import Ki qualified
import Numeric.Natural (Natural)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Random.Stateful (globalStdGen, uniformRM)
import Text.Printf (printf)
import Zmq qualified
import Zmq.Dealer qualified
import Zmq.Pub qualified
import Zmq.Pull qualified
import Zmq.Push qualified
import Zmq.Rep qualified
import Zmq.Req qualified
import Zmq.Router qualified
import Zmq.Sub qualified
import Zmq.XPub qualified
import Zmq.XSub qualified
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
    ["syncpub"] -> syncpub
    ["syncsub"] -> syncsub
    ["psenvpub"] -> psenvpub
    ["psenvsub"] -> psenvsub
    ["rtreq"] -> rtreq
    ["rtdealer"] -> rtdealer
    ["lbbroker"] -> lbbroker
    ["asyncsrv"] -> asyncsrv
    "peering1" : self : peers -> peering1 self peers
    _ -> pure ()

-- Hello World server
hwserver :: IO ()
hwserver =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to clients
    responder <- unwrap (Zmq.Rep.open (Zmq.name "responder"))
    unwrap (Zmq.bind responder "tcp://*:5555")

    forever do
      _ <- unwrap (Zmq.receive responder)
      putStrLn "Received Hello"
      threadDelay 1_000_000 -- Do some work
      unwrap (Zmq.send responder "World")

-- Hello World client
hwclient :: IO ()
hwclient =
  Zmq.run Zmq.defaultOptions do
    putStrLn "Connecting to hello world server..."
    requester <- unwrap (Zmq.Req.open (Zmq.name "requester"))
    unwrap (Zmq.connect requester "tcp://localhost:5555")

    for_ [(0 :: Int) .. 9] \requestNbr -> do
      printf "Sending Hello %d...\n" requestNbr
      unwrap (Zmq.send requester "Hello")
      _ <- unwrap (Zmq.receive requester)
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
    publisher <- unwrap (Zmq.Pub.open (Zmq.name "publisher"))
    unwrap (Zmq.bind publisher "tcp://*:5556")

    forever do
      -- Get values that will fool the boss
      zipcode <- uniformRM (0 :: Int, 99999) globalStdGen
      temperature <- uniformRM (-80 :: Int, 134) globalStdGen
      relhumidity <- uniformRM (10 :: Int, 59) globalStdGen

      -- Send message to all subscribers
      let update = ByteString.Char8.pack (printf "%05d %d %d" zipcode temperature relhumidity)
      unwrap (Zmq.send publisher update)

-- Weather update client
-- Connects SUB socket to tcp://localhost:5556
-- Collects weather updates and finds avg temp in zipcode
wuclient :: IO ()
wuclient =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to server
    putStrLn "Collecting updates from weather server..."
    subscriber <- unwrap (Zmq.Sub.open (Zmq.name "subscriber"))
    unwrap (Zmq.connect subscriber "tcp://localhost:5556")

    -- Subscribe to zipcode, default is NYC, 10001
    filter <-
      getArgs <&> \case
        [] -> "10001 "
        filter : _ -> filter
    unwrap (Zmq.Sub.subscribe subscriber (ByteString.Char8.pack filter))

    -- Process 100 updates
    temps <-
      replicateM 100 do
        string <- unwrap (Zmq.receive subscriber)
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
    sender <- unwrap (Zmq.Push.open (Zmq.name "sender"))
    unwrap (Zmq.bind sender "tcp://*:5557")

    -- Socket to send start of batch message on
    sink <- unwrap (Zmq.Push.open (Zmq.name "sink"))
    unwrap (Zmq.connect sink "tcp://localhost:5558")

    putStrLn "Press Enter when the workers are ready"
    _ <- getLine
    putStrLn "Sending tasks to workers..."

    -- The first message is "0" and signals start of batch
    unwrap (Zmq.send sink "0")

    -- Send 100 tasks
    workloads <-
      replicateM 100 do
        -- Random workload from 1 to 100msecs
        workload <- uniformRM (1 :: Int, 100) globalStdGen
        unwrap (Zmq.send sender (ByteString.Char8.pack (printf "%d" workload)))
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
    receiver <- unwrap (Zmq.Pull.open (Zmq.name "receiver"))
    unwrap (Zmq.connect receiver "tcp://localhost:5557")

    -- Socket to send messages to
    sender <- unwrap (Zmq.Push.open (Zmq.name "sender"))
    unwrap (Zmq.connect sender "tcp://localhost:5558")

    -- Process tasks forever
    forever do
      string <- unwrap (Zmq.receive receiver)
      printf "%s." (ByteString.Char8.unpack string) -- Show progress
      hFlush stdout
      threadDelay (read (ByteString.Char8.unpack string) * 1_000) -- Do the work
      unwrap (Zmq.send sender "")

-- Task sink
-- Binds PULL socket to tcp://localhost:5558
-- Collects results from workers via that socket
tasksink :: IO ()
tasksink =
  Zmq.run Zmq.defaultOptions do
    -- Prepare our socket
    receiver <- unwrap (Zmq.Pull.open (Zmq.name "receiver"))
    unwrap (Zmq.bind receiver "tcp://*:5558")

    -- Wait for start of batch
    _ <- unwrap (Zmq.receive receiver)

    -- Start our clock now
    startTime <- getMonotonicTimeNSec

    -- Process 100 confirmations
    for_ [(0 :: Int) .. 99] \taskNbr -> do
      _ <- unwrap (Zmq.receive receiver)
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
    receiver <- unwrap (Zmq.Pull.open (Zmq.name "receiver"))
    unwrap (Zmq.connect receiver "tcp://localhost:5557")

    -- Connect to weather server
    subscriber <- unwrap (Zmq.Sub.open (Zmq.name "subscriber"))
    unwrap (Zmq.connect subscriber "tcp://localhost:5556")
    unwrap (Zmq.Sub.subscribe subscriber "10001 ")

    -- Process messages from both sockets
    forever do
      let items =
            Zmq.the receiver
              & Zmq.also subscriber
      Zmq.Ready ready <- unwrap (Zmq.poll items)
      when (ready receiver) do
        Zmq.receive receiver >>= \case
          Left _ -> pure ()
          Right _ ->
            -- Process task
            pure ()
      when (ready subscriber) do
        Zmq.receive subscriber >>= \case
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
    requester <- unwrap (Zmq.Req.open (Zmq.name "requester"))
    unwrap (Zmq.connect requester "tcp://localhost:5559")

    for_ [(0 :: Int) .. 9] \requestNbr -> do
      unwrap (Zmq.send requester "Hello")
      string <- unwrap (Zmq.receive requester)
      printf "Received reply %d [%s]\n" requestNbr (ByteString.Char8.unpack string)

-- Hello World worker
-- Connects REP socket to tcp://localhost:5560
-- Expects "Hello" from client, replies with "World"
rrworker :: IO ()
rrworker =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to clients
    responder <- unwrap (Zmq.Rep.open (Zmq.name "responder"))
    unwrap (Zmq.connect responder "tcp://localhost:5560")

    forever do
      -- Wait for next request from client
      string <- unwrap (Zmq.receive responder)
      printf "Received request: [%s]\n" (ByteString.Char8.unpack string)

      -- Do some 'work'
      threadDelay 1_000_000

      -- Send reply back to client
      unwrap (Zmq.send responder "World")

-- Simple request-reply broker
rrbroker :: IO ()
rrbroker =
  Zmq.run Zmq.defaultOptions do
    -- Prepare our sockets
    frontend <- unwrap (Zmq.Router.open (Zmq.name "frontend"))
    backend <- unwrap (Zmq.Dealer.open (Zmq.name "backend"))
    unwrap (Zmq.bind frontend "tcp://*:5559")
    unwrap (Zmq.bind backend "tcp://*:5560")

    -- Initialize poll set
    let items =
          Zmq.the frontend
            & Zmq.also backend
    -- Switch messages between sockets
    forever do
      Zmq.Ready ready <- unwrap (Zmq.poll items)
      when (ready frontend) do
        message <- unwrap (Zmq.receives frontend)
        unwrap (Zmq.Dealer.sends backend message)
      when (ready backend) do
        message <- unwrap (Zmq.receives backend)
        unwrap (Zmq.Router.sends frontend message)

-- Weather proxy device
wuproxy :: IO ()
wuproxy =
  Zmq.run Zmq.defaultOptions do
    -- This is where the weather server sits
    frontend <- unwrap (Zmq.XSub.open (Zmq.name "frontend"))
    unwrap (Zmq.connect frontend "tcp://192.168.55.210:5556")

    -- This is our public endpoint for subscribers
    backend <- unwrap (Zmq.XPub.open (Zmq.name "backend"))
    unwrap (Zmq.bind backend "tcp://10.1.1.0:8100")

    -- Run the proxy until the user interrupts us
    let items =
          Zmq.the frontend
            & Zmq.also backend
    forever do
      Zmq.Ready ready <- unwrap (Zmq.poll items)
      when (ready frontend) do
        message <- unwrap (Zmq.receives frontend)
        unwrap (Zmq.XPub.sends backend message)
      when (ready backend) do
        message <- unwrap (Zmq.receives backend)
        unwrap (Zmq.XSub.sends frontend message)

-- Task worker - design 2
-- Adds pub-sub flow to receive and respond to kill signal
taskwork2 :: IO ()
taskwork2 =
  Zmq.run Zmq.defaultOptions do
    -- Socket to receive messages on
    receiver <- unwrap (Zmq.Pull.open (Zmq.name "receiver"))
    unwrap (Zmq.connect receiver "tcp://localhost:5557")

    -- Socket to send messages to
    sender <- unwrap (Zmq.Push.open (Zmq.name "sender"))
    unwrap (Zmq.connect sender "tcp://localhost:5558")

    -- Socket for control input
    controller <- unwrap (Zmq.Sub.open (Zmq.name "controller"))
    unwrap (Zmq.connect controller "tcp://localhost:5559")
    unwrap (Zmq.Sub.subscribe controller "")

    -- Process messages from either socket
    let loop = do
          let items =
                Zmq.the receiver
                  & Zmq.also controller
          Zmq.Ready ready <- unwrap (Zmq.poll items)
          when (ready receiver) do
            string <- unwrap (Zmq.receive receiver)
            printf "%s." (ByteString.Char8.unpack string) -- Show progress
            hFlush stdout
            threadDelay (read (ByteString.Char8.unpack string) * 1_000) -- Do the work
            unwrap (Zmq.send sender "")
          -- Any waiting controller command acts as 'KILL'
          when (not (ready controller)) do
            loop
    loop

-- Task sink - design 2
-- Adds pub-sub flow to send kill signal to workers
tasksink2 :: IO ()
tasksink2 =
  Zmq.run Zmq.defaultOptions do
    -- Socket to receive messages on
    receiver <- unwrap (Zmq.Pull.open (Zmq.name "receiver"))
    unwrap (Zmq.bind receiver "tcp://*:5558")

    -- Socket for worker control
    controller <- unwrap (Zmq.Pub.open (Zmq.name "controller"))
    unwrap (Zmq.bind controller "tcp://*:5559")

    -- Wait for start of batch
    _ <- unwrap (Zmq.receive receiver)

    -- Start our clock now
    startTime <- getMonotonicTimeNSec

    -- Process 100 confirmations
    for_ [(0 :: Int) .. 99] \taskNbr -> do
      _ <- unwrap (Zmq.receive receiver)
      putChar (if mod taskNbr 10 == 0 then ':' else '.')
      hFlush stdout
    stopTime <- getMonotonicTimeNSec
    printf "Total elapsed time: %d msec\n" ((stopTime - startTime) `div` 1_000_000)

    -- Send kill signal to workers
    unwrap (Zmq.send controller "KILL")

-- Multithreaded Hello World server
mtserver :: IO ()
mtserver =
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to clients
    clients <- unwrap (Zmq.Router.open (Zmq.name "clients"))
    unwrap (Zmq.bind clients "tcp://*:5555")

    -- Socket to talk to workers
    workers <- unwrap (Zmq.Dealer.open (Zmq.name "workers"))
    unwrap (Zmq.bind workers "inproc://workers")

    -- Launch pool of worker threads
    Ki.scoped \scope -> do
      replicateM_ 5 do
        Ki.fork_ scope do
          -- Socket to talk to dispatcher
          receiver <- unwrap (Zmq.Rep.open (Zmq.name "receiver"))
          unwrap (Zmq.connect receiver "inproc://workers")

          forever do
            string <- unwrap (Zmq.receive receiver)
            printf "Received request: [%s]\n" (ByteString.Char8.unpack string)
            -- Do some 'work'
            threadDelay 1_000_000
            -- Send reply back to client
            unwrap (Zmq.send receiver "World")
      -- Connect work threads to client threads via a queue proxy
      let items =
            Zmq.the clients
              & Zmq.also workers
      forever do
        Zmq.Ready ready <- unwrap (Zmq.poll items)
        when (ready clients) do
          message <- unwrap (Zmq.receives clients)
          unwrap (Zmq.Dealer.sends workers message)
        when (ready workers) do
          message <- unwrap (Zmq.receives workers)
          unwrap (Zmq.Router.sends clients message)

-- Synchronized publisher
syncpub :: IO ()
syncpub = do
  let subscribersExpected = 10 :: Int -- We wait for 10 subscribers
  Zmq.run Zmq.defaultOptions do
    -- Socket to talk to clients
    let sndhwm = 1_100_000 :: Natural
    publisher <- unwrap (Zmq.Pub.open (Zmq.name "publisher" <> Zmq.sendQueueSize sndhwm))

    unwrap (Zmq.bind publisher "tcp://*:5561")

    -- Socket to receive signals
    syncservice <- unwrap (Zmq.Rep.open (Zmq.name "syncservice"))
    unwrap (Zmq.bind syncservice "tcp://*:5562")

    -- Get synchronization from subscribers
    putStrLn "Waiting for subscribers"
    replicateM_ subscribersExpected do
      -- wait for synchronization request
      _ <- unwrap (Zmq.receive syncservice)
      -- send synchronization reply
      unwrap (Zmq.send syncservice "")
    -- Now broadcast exactly 1M updates followed by END
    putStrLn "Broadcasting messages"
    replicateM_ 1_000_000 do
      unwrap (Zmq.send publisher "Rhubarb")

    unwrap (Zmq.send publisher "END")

-- Synchronized subscriber
syncsub :: IO ()
syncsub = do
  Zmq.run Zmq.defaultOptions do
    -- First, connect our subscriber socket
    subscriber <- unwrap (Zmq.Sub.open (Zmq.name "subscriber"))
    unwrap (Zmq.connect subscriber "tcp://localhost:5561")
    unwrap (Zmq.Sub.subscribe subscriber "")

    -- 0MQ is so fast, we need to wait a while...
    threadDelay 1_000_000

    -- Second, synchronize with publisher
    syncclient <- unwrap (Zmq.Req.open (Zmq.name "syncclient"))
    unwrap (Zmq.connect syncclient "tcp://localhost:5562")

    -- send a synchronization request
    unwrap (Zmq.send syncclient "")

    -- wait for synchronization reply
    _ <- unwrap (Zmq.receive syncclient)

    -- Third, get our updates and report how many we got
    let loop updateNbr = do
          string <- unwrap (Zmq.receive subscriber)
          if string /= "END"
            then loop (updateNbr + 1)
            else pure updateNbr
    updateNbr <- loop (0 :: Int)
    printf "Received %d updates\n" updateNbr

-- Pubsub envelope publisher
psenvpub :: IO ()
psenvpub =
  Zmq.run Zmq.defaultOptions do
    -- Prepare our publisher
    publisher <- unwrap (Zmq.Pub.open (Zmq.name "publisher"))
    unwrap (Zmq.bind publisher "tcp://*:5563")

    forever do
      -- Write two messages, each with an envelope and content
      unwrap (Zmq.Pub.sends publisher ["A", "We don't want to see this"])
      unwrap (Zmq.Pub.sends publisher ["B", "We would like to see this"])
      threadDelay 1_000_000

-- Pubsub envelope subscriber
psenvsub :: IO ()
psenvsub =
  Zmq.run Zmq.defaultOptions do
    -- Prepare our subscriber
    subscriber <- unwrap (Zmq.Sub.open (Zmq.name "subscriber"))
    unwrap (Zmq.connect subscriber "tcp://localhost:5563")
    unwrap (Zmq.Sub.subscribe subscriber "B")

    forever do
      -- Read envelope with address and message contents
      unwrap (Zmq.receives subscriber) >>= \case
        [address, contents] -> printf "[%s] %s\n" (ByteString.Char8.unpack address) (ByteString.Char8.unpack contents)
        _ -> pure ()

-- ROUTER-to-REQ example
rtreq :: IO ()
rtreq = do
  let nbrWorkers = 10 :: Int

  Zmq.run Zmq.defaultOptions do
    broker <- unwrap (Zmq.Router.open (Zmq.name "broker"))

    unwrap (Zmq.bind broker "tcp://*:5671")

    Ki.scoped \scope -> do
      replicateM_ nbrWorkers do
        _ <- Ki.fork scope workerTask
        pure ()
      -- Run for five seconds and then tell workers to end
      endTime <- (+ 5_000_000_000) <$> getMonotonicTimeNSec
      let loop workersFired = do
            -- Next message gives us least recently used worker
            message <- unwrap (Zmq.receives broker)
            -- Encourage workers until it's time to fire them
            time <- getMonotonicTimeNSec
            if time < endTime
              then do
                unwrap (Zmq.Router.sends broker (take 2 message ++ ["", "Work harder"]))
                loop workersFired
              else do
                unwrap (Zmq.Router.sends broker (take 2 message ++ ["", "Fired!"]))
                when (workersFired + 1 < nbrWorkers) do
                  loop (workersFired + 1)
      loop 0
      atomically (Ki.awaitAll scope)
  where
    workerTask :: IO ()
    workerTask = do
      worker <- unwrap (Zmq.Req.open (Zmq.name "worker"))
      unwrap (Zmq.connect worker "tcp://localhost:5671")

      let loop total = do
            -- Tell the broker we're ready for work
            unwrap (Zmq.send worker "Hi Boss")

            -- Get workload from broker, until finished
            workload <- unwrap (Zmq.receive worker)
            let finished = workload == "Fired!"
            if finished
              then printf "Completed: %d tasks\n" total
              else do
                -- Do some random work
                threadDelay =<< uniformRM (1_000, 500_000) globalStdGen
                loop (total + 1)
      loop (0 :: Int)

-- ROUTER-to-DEALER example
rtdealer :: IO ()
rtdealer = do
  let nbrWorkers = 1 :: Int

  Zmq.run Zmq.defaultOptions do
    broker <- unwrap (Zmq.Router.open (Zmq.name "broker"))

    unwrap (Zmq.bind broker "tcp://*:5671")

    Ki.scoped \scope -> do
      for_ [1 .. nbrWorkers] \workerNbr -> do
        _ <- Ki.fork scope (workerTask workerNbr)
        pure ()
      -- Run for five seconds and then tell workers to end
      endTime <- (+ 5_000_000_000) <$> getMonotonicTimeNSec
      let loop workersFired = do
            -- Next message gives us least recently used worker
            message <- unwrap (Zmq.receives broker)
            -- Encourage workers until it's time to fire them
            time <- getMonotonicTimeNSec
            if time < endTime
              then do
                unwrap (Zmq.Router.sends broker (take 1 message ++ ["", "Work harder"]))
                loop workersFired
              else do
                unwrap (Zmq.Router.sends broker (take 1 message ++ ["", "Fired!"]))
                when (workersFired + 1 < nbrWorkers) do
                  loop (workersFired + 1)
      loop 0
      atomically (Ki.awaitAll scope)
  where
    workerTask :: Int -> IO ()
    workerTask workerNbr = do
      worker <- unwrap (Zmq.Dealer.open (Zmq.name ("worker " <> Text.pack (show workerNbr))))
      unwrap (Zmq.connect worker "tcp://localhost:5671")

      let loop total = do
            -- Tell the broker we're ready for work
            unwrap (Zmq.Dealer.sends worker ["", "Hi Boss"])

            -- Get workload from broker, until finished
            unwrap (Zmq.receives worker) >>= \case
              ["", "Fired!"] -> printf "Completed: %d tasks\n" total
              _ -> do
                -- Do some random work
                threadDelay =<< uniformRM (1_000, 500_000) globalStdGen
                loop (total + 1)
      loop (0 :: Int)

-- Load-balancing broker
-- This is the main task. It starts the clients and workers, and then
-- routes requests between the two layers. Workers signal READY when
-- they start; after that we treat them as ready when they reply with
-- a response back to a client. The load-balancing data structure is
-- just a queue of next available workers.
lbbroker :: IO ()
lbbroker =
  Zmq.run Zmq.defaultOptions do
    let nbrClients = 10 :: Int
    let nbrWorkers = 3 :: Int

    -- Prepare our sockets
    frontend <- unwrap (Zmq.Router.open (Zmq.name "frontend"))
    backend <- unwrap (Zmq.Router.open (Zmq.name "backend"))

    unwrap (Zmq.bind frontend "ipc://frontend.ipc")
    unwrap (Zmq.bind backend "ipc://backend.ipc")

    Ki.scoped \scope -> do
      for_ [1 .. nbrClients] \clientNbr -> do
        _ <- Ki.fork scope (clientTask clientNbr)
        pure ()
      for_ [1 .. nbrWorkers] \workerNbr -> do
        _ <- Ki.fork scope (workerTask workerNbr)
        pure ()
      -- Here is the main loop for the least-recently-used queue. It has two
      -- sockets; a frontend for clients and a backend for workers. It polls
      -- the backend in all cases, and polls the frontend only when there are
      -- one or more workers ready. This is a neat way to use 0MQ's own queues
      -- to hold messages we're not ready to process yet. When we get a client
      -- request, we pop the next available worker and send the request to it,
      -- including the originating client identity. When a worker replies, we
      -- requeue that worker and forward the reply to the original client
      -- using the reply envelope.

      let loop clientNbr workerQueue = do
            let items =
                  Zmq.the backend
                    -- Poll frontend only if we have available workers
                    & if not (null workerQueue) then Zmq.also frontend else id
            Zmq.poll items >>= \case
              Left _ -> pure () -- Interrupted
              Right (Zmq.Ready ready) -> do
                -- Handle worker activity on backend
                (clientNbr1, workerQueue1) <-
                  if ready backend
                    then do
                      -- Queue worker identity for load-balancing
                      -- Second frame is request id (ZMQ_REQ_CORRELATE)
                      -- Third frame is empty
                      -- Fourth frame is READY or else a client reply identity
                      unwrap (Zmq.receives backend) >>= \case
                        [workerId, workerRequestId, "", "READY"] ->
                          pure (clientNbr, (workerId, workerRequestId) : workerQueue)
                        -- If client reply, send rest back to frontend
                        [workerId, workerRequestId, "", clientId, clientRequestId, "", reply] -> do
                          unwrap (Zmq.Router.sends frontend [clientId, clientRequestId, "", reply])
                          pure (clientNbr - 1, (workerId, workerRequestId) : workerQueue)
                        _ -> pure (clientNbr, workerQueue)
                    else pure (clientNbr, workerQueue)

                when (clientNbr1 > 0) do
                  workerQueue2 <-
                    case (ready frontend, workerQueue1) of
                      -- Here is how we handle a client request:
                      (True, (workerId, workerRequestId) : workerQueue2) -> do
                        -- Now get next client request, route to last-used worker
                        -- Client request is [identity][messageid][empty][request]
                        unwrap (Zmq.receives frontend) >>= \case
                          [clientId, clientRequestId, "", request] -> do
                            unwrap $
                              Zmq.Router.sends
                                backend
                                [ workerId,
                                  workerRequestId,
                                  "",
                                  clientId,
                                  clientRequestId,
                                  "",
                                  request
                                ]
                            pure workerQueue2
                          _ -> pure workerQueue1
                      _ -> pure workerQueue1
                  loop clientNbr1 workerQueue2
      loop nbrClients []
  where
    -- Basic request-reply client using REQ socket
    clientTask :: Int -> IO ()
    clientTask clientNbr = do
      client <- unwrap (Zmq.Req.open (Zmq.name ("client " <> Text.pack (show clientNbr))))
      unwrap (Zmq.connect client "ipc://frontend.ipc")

      -- Send request, get reply
      unwrap (Zmq.send client "HELLO")
      reply <- unwrap (Zmq.receive client)
      printf "Client: %s\n" (ByteString.Char8.unpack reply)

    -- This is the worker task, using a REQ socket to do load-balancing.
    workerTask :: Int -> IO ()
    workerTask workerNbr = do
      worker <- unwrap (Zmq.Req.open (Zmq.name ("worker " <> Text.pack (show workerNbr))))
      unwrap (Zmq.connect worker "ipc://backend.ipc")

      -- Tell broker we're ready for work
      unwrap (Zmq.send worker "READY")

      forever do
        -- Read and save all frames until we get an empty frame
        -- In this example there are only 2, but there could be more
        unwrap (Zmq.receives worker) >>= \case
          [clientId, clientRequestId, "", request] -> do
            -- Send reply
            printf "Worker: %s\n" (ByteString.Char8.unpack request)
            unwrap (Zmq.Req.sends worker [clientId, clientRequestId, "", "OK"])
          _ -> pure ()

-- Asynchronous client-to-server (DEALER to ROUTER)
--
-- While this example runs in a single process, that is to make
-- it easier to start and stop the example. Each task conceptually
-- acts as a separate process.
--
-- The main thread simply starts several clients and a server, and then
-- waits for the server to finish.
asyncsrv :: IO ()
asyncsrv =
  Zmq.run Zmq.defaultOptions do
    Ki.scoped \scope -> do
      for_ [(1 :: Int) .. 3] \clientNbr ->
        Ki.fork_ scope (clientTask clientNbr)
      Ki.fork_ scope serverTask

      threadDelay 5_000_000 -- Run for 5 seconds then quit
  where
    -- This is our client task
    -- It connects to the server, and then sends a request once per second
    -- It collects responses as they arrive, and it prints them out. We will
    -- run several client tasks in parallel, each with a different random ID.
    clientTask :: Int -> IO Void
    clientTask clientNbr = do
      client <- unwrap (Zmq.Dealer.open (Zmq.name ("client " <> Text.pack (show clientNbr))))

      unwrap (Zmq.connect client "tcp://localhost:5570")

      Ki.scoped \scope -> do
        Ki.fork_ scope do
          forever do
            msg <- unwrap (Zmq.receive client)
            print msg
        let loop requestNbr = do
              threadDelay 1_000_000
              unwrap (Zmq.send client ("request #" <> ByteString.Char8.pack (show requestNbr)))
              loop (requestNbr + 1)
        loop (1 :: Int)

    -- This is our server task.
    -- It uses the multithreaded server model to deal requests out to a pool
    -- of workers and route replies back to clients. One worker can handle
    -- one request at a time but one client can talk to multiple workers at
    -- once.
    serverTask :: IO Void
    serverTask = do
      -- Launch pool of worker threads, precise number is not critical
      let nbrThreads = 5 :: Int
      Ki.scoped \scope -> do
        for_ [1 .. nbrThreads] \threadNbr -> do
          Ki.fork_ scope (serverWorker threadNbr)

        -- Connect backend to frontend
        frontend <- unwrap (Zmq.Router.open (Zmq.name "frontend"))
        unwrap (Zmq.bind frontend "tcp://*:5570")

        backend <- unwrap (Zmq.Dealer.open (Zmq.name "backend"))
        unwrap (Zmq.bind backend "inproc://backend")

        forever do
          let items =
                Zmq.the frontend
                  & Zmq.also backend
          Zmq.Ready ready <- unwrap (Zmq.poll items)
          when (ready frontend) do
            message <- unwrap (Zmq.receives frontend)
            unwrap (Zmq.Dealer.sends backend message)
          when (ready backend) do
            message <- unwrap (Zmq.receives backend)
            unwrap (Zmq.Router.sends frontend message)

    -- Each worker task works on one request at a time and sends a random number
    -- of replies back, with random delays between replies:
    serverWorker :: Int -> IO Void
    serverWorker threadNbr = do
      worker <- unwrap (Zmq.Dealer.open (Zmq.name ("worker " <> Text.pack (show threadNbr))))
      unwrap (Zmq.connect worker "inproc://backend")

      forever do
        unwrap (Zmq.receives worker) >>= \case
          [identity, content] -> do
            -- Send 0..4 replies back
            replies <- uniformRM (0 :: Int, 4) globalStdGen
            replicateM_ replies do
              -- Sleep for some fraction of a second
              threadDelay =<< uniformRM (1_000, 1_000_000) globalStdGen
              unwrap (Zmq.Dealer.sends worker [identity, content])
          _ -> pure ()

-- Broker peering simulation (part 1)
-- Prototypes the state flow
-- First argument is this broker's name
-- Other arguments are our peers' names
peering1 :: String -> [String] -> IO ()
peering1 self peers =
  Zmq.run Zmq.defaultOptions do
    putStrLn ("I: preparing broker at " ++ self ++ "...")

    -- Bind state backend to endpoint
    statebe <- unwrap (Zmq.Pub.open (Zmq.name "statebe"))
    unwrap (Zmq.bind statebe ("ipc://" <> Text.pack self <> "-state.ipc"))

    -- Connect statefe to all peers
    statefe <- unwrap (Zmq.Sub.open (Zmq.name "statefe"))
    unwrap (Zmq.Sub.subscribe statefe "")
    for_ peers \peer -> do
      putStrLn ("I: connecting to state backend at '" ++ peer ++ "'")
      unwrap (Zmq.connect statefe ("ipc://" <> Text.pack peer <> "-state.ipc"))

    -- The main loop sends out status messages to peers, and collects
    -- status messages back from peers. The zmq_poll timeout defines
    -- our own heartbeat:
    let loop = do
          -- Poll for activity, or 1 second timeout
          Zmq.pollFor (Zmq.the statefe) 1_000 >>= \case
            Left _ -> pure () -- Interrupted
            Right maybeReady -> do
              case maybeReady of
                -- Handle incoming status messages
                Just _ready ->
                  unwrap (Zmq.receives statefe) >>= \case
                    [peerName, available] ->
                      printf
                        "%s - %s workers free\n"
                        (ByteString.Char8.unpack peerName)
                        (ByteString.Char8.unpack available)
                    _ -> pure ()
                -- Send random values for worker availability
                Nothing -> do
                  val <- uniformRM (0 :: Int, 9) globalStdGen
                  unwrap $
                    Zmq.Pub.sends
                      statebe
                      [ ByteString.Char8.pack self,
                        ByteString.Char8.pack (show val)
                      ]
              loop
    loop

------------------------------------------------------------------------------------------------------------------------
-- Utils

unwrap :: IO (Either Zmq.Error a) -> IO a
unwrap action =
  action >>= \case
    Left err -> throwIO err
    Right value -> pure value
