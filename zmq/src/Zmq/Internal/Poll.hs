{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Zmq.Internal.Poll
  ( CanPoll,
    Sockets,
    Ready (..),
    the,
    also,
    poll,
    pollFor,
    pollUntil,
  )
where

import Control.Exception
import Data.Array (Array)
import Data.Array.Base qualified as Array
import Data.Array.MArray qualified as MArray
import Data.Array.Storable (StorableArray)
import Data.Int (Int64)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Libzmq
import Libzmq.Bindings qualified
import Zmq.Error (Error, enrichError, unexpectedError)
import Zmq.Internal.Socket (Socket (..))
import Zmq.Internal.Socket qualified as Socket

class CanPoll a

instance CanPoll "DEALER"

instance CanPoll "PAIR"

instance CanPoll "PULL"

instance CanPoll "REP"

instance CanPoll "REQ"

instance CanPoll "ROUTER"

instance CanPoll "SUB"

instance CanPoll "XPUB"

instance CanPoll "XSUB"

data Sockets
  = Sockets
      -- pollables in reverse order of how they were added (with postfix syntax), e.g.
      -- the X & also Y & also Z = [Z, Y, X]
      ![P]
      -- length of pollables
      !Int

data Ready
  = Ready (forall a. Socket a -> Bool)

data P
  = forall a. P (Socket a)

instance Eq P where
  P x == P y = zsocket x == zsocket y

-- TODO other methods
instance Ord P where
  compare (P x) (P y) = compare (zsocket x) (zsocket y)

ppollitem :: P -> Zmq_pollitem
ppollitem (P Socket {zsocket}) =
  Zmq_pollitem_socket zsocket ZMQ_POLLIN

the :: CanPoll a => Socket a -> Sockets
the socket =
  Sockets [P socket] 1

also :: CanPoll a => Socket a -> Sockets -> Sockets
also socket (Sockets pollables len) =
  Sockets (P socket : pollables) (len + 1)

-- Keep sockets alive for duration of IO action
keepingSocketsAlive :: Array Int P -> IO a -> IO a
keepingSocketsAlive sockets action = do
  let (lo, hi) = Array.bounds sockets
  let go !i =
        if i > hi
          then action
          else case sockets Array.! i of
            P Socket {zsocket} ->
              Socket.zhs_keepalive zsocket (go (i + 1))
  go lo

data PreparedSockets = PreparedSockets
  { -- The subset of the input sockets that actually need to be polled. Each is either a non-REQ socket, or a REQ socket
    -- with an empty message buffer.
    socketsToPoll :: Array Int P,
    -- The same sockets as `socketsToPoll`, to pass to libzmq
    socketsToPoll2 :: StorableArray Int Zmq_pollitem
  }

prepareSockets :: Sockets -> IO PreparedSockets
prepareSockets (Sockets sockets0 len) = do
  let sockets1 = reverse sockets0
  socketsToPoll2 <- MArray.newListArray (0, len - 1) (map ppollitem sockets1)
  pure
    PreparedSockets
      { socketsToPoll = Array.listArray (0, len - 1) sockets1,
        socketsToPoll2
      }

socketsArrayPs :: PreparedSockets -> IO Ready
socketsArrayPs PreparedSockets {socketsToPoll, socketsToPoll2} = do
  (lo, hi) <- MArray.getBounds socketsToPoll2
  let loop :: Set P -> Int -> IO Ready
      loop !acc !i =
        if i > hi
          then pure (Ready \socket -> Set.member (P socket) acc)
          else do
            pollitem <- Array.unsafeRead socketsToPoll2 i
            if Libzmq.Bindings.revents pollitem == 0
              then loop acc (i + 1)
              else loop (Set.insert (socketsToPoll Array.! i) acc) (i + 1)
  loop Set.empty lo

-- TODO make Sockets wrap the StorableArray so we don't allocate it anew each time
poll :: Sockets -> IO (Either Error Ready)
poll sockets = do
  preparedSockets <- prepareSockets sockets
  keepingSocketsAlive (socketsToPoll preparedSockets) do
    -- Poll indefinitely, unless we already have at least one full REQ socket, in which case we do a non-blocking poll
    let timeout = if False then (-1) else (0 :: Int64)
    zhs_poll (socketsToPoll2 preparedSockets) timeout >>= \case
      Left err -> pure (Left err)
      Right _n -> Right <$> socketsArrayPs preparedSockets

-- | milliseconds
pollFor :: Sockets -> Int -> IO (Either Error (Maybe Ready))
pollFor sockets timeout =
  pollFor_ sockets (fromIntegral @Int @Int64 timeout)

pollFor_ :: Sockets -> Int64 -> IO (Either Error (Maybe Ready))
pollFor_ sockets timeout = do
  preparedSockets <- prepareSockets sockets
  keepingSocketsAlive (socketsToPoll preparedSockets) do
    zhs_poll (socketsToPoll2 preparedSockets) timeout >>= \case
      Left err -> pure (Left err)
      Right n ->
        if n == 0
          then pure (Right Nothing)
          else Right . Just <$> socketsArrayPs preparedSockets

-- | monotonic time as reported by 'getMonotonicTimeNSec'
pollUntil :: Sockets -> Word64 -> IO (Either Error (Maybe Ready))
pollUntil sockets deadline = do
  now <- getMonotonicTimeNSec
  let timeout =
        if now > deadline
          then 0
          else -- safe downcast: can't overflow Int64 after dividing by 1,000,000
            fromIntegral @Word64 @Int64 ((deadline - now) `div` 1_000_000)
  pollFor_ sockets timeout

zhs_poll :: StorableArray Int Zmq_pollitem -> Int64 -> IO (Either Error Int)
zhs_poll pollitems timeout = do
  zmq_poll pollitems timeout >>= \case
    Left errno ->
      let err = enrichError "zmq_poll" errno
       in case errno of
            EINTR -> pure (Left err)
            EFAULT -> throwIO err
            ETERM -> pure (Left err)
            _ -> unexpectedError err
    Right n -> pure (Right n)
