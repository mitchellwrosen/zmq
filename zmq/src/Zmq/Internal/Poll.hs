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
import Data.Array.Base qualified as Array
import Data.Array.MArray qualified as MArray
import Data.Array.Storable (StorableArray)
import Data.Int (Int64)
import Data.Primitive.Array qualified as Primitive (Array)
import Data.Primitive.Array qualified as Primitive.Array
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Libzmq
import Libzmq.Bindings qualified
import Zmq.Error (Error, enrichError, unexpectedError)
import Zmq.Internal.IO (keepAlive)
import Zmq.Internal.Socket (Socket (..))
import Zmq.Internal.SomeSocket (SomeSocket (..))

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
      -- socket in reverse order of how they were added (with postfix syntax), e.g. the X & also Y & also Z = [Z, Y, X]
      ![SomeSocket]
      -- number of sockets in the list
      !Int

data Ready
  = Ready (forall a. Socket a -> Bool)

the :: CanPoll a => Socket a -> Sockets
the socket =
  Sockets [SomeSocket socket] 1

also :: CanPoll a => Socket a -> Sockets -> Sockets
also socket (Sockets sockets len) =
  Sockets (SomeSocket socket : sockets) (len + 1)

------------------------------------------------------------------------------------------------------------------------
-- Preparing sockets for polling

data PreparedSockets = PreparedSockets
  { -- The subset of the input sockets that actually need to be polled. Each is either a non-REQ socket, or a REQ socket
    -- with an empty message buffer.
    socketsToPoll :: Primitive.Array SomeSocket,
    -- The same sockets as `socketsToPoll`, to pass to libzmq
    socketsToPoll2 :: StorableArray Int Zmq_pollitem
  }

prepareSockets :: Sockets -> IO PreparedSockets
prepareSockets (Sockets sockets0 len) = do
  let sockets1 = reverse sockets0
  socketsToPoll2 <- MArray.newListArray (0, len - 1) (map someSocketToPollitem sockets1)
  pure
    PreparedSockets
      { socketsToPoll = Primitive.Array.arrayFromListN len sockets1,
        socketsToPoll2
      }

someSocketToPollitem :: SomeSocket -> Zmq_pollitem
someSocketToPollitem (SomeSocket Socket {zsocket}) =
  Zmq_pollitem_socket zsocket ZMQ_POLLIN

readySockets :: PreparedSockets -> IO Ready
readySockets PreparedSockets {socketsToPoll, socketsToPoll2} = do
  (lo, hi) <- MArray.getBounds socketsToPoll2
  let loop :: Set SomeSocket -> Int -> IO Ready
      loop !acc !i =
        if i > hi
          then pure (Ready \socket -> Set.member (SomeSocket socket) acc)
          else do
            pollitem <- Array.unsafeRead socketsToPoll2 i
            if Libzmq.Bindings.revents pollitem == 0
              then loop acc (i + 1)
              else loop (Set.insert (Primitive.Array.indexArray socketsToPoll i) acc) (i + 1)
  loop Set.empty lo

------------------------------------------------------------------------------------------------------------------------
-- Polling

-- TODO make Sockets wrap the StorableArray so we don't allocate it anew each time
poll :: Sockets -> IO (Either Error Ready)
poll sockets = do
  preparedSockets <- prepareSockets sockets
  keepAlive sockets do
    -- Poll indefinitely, unless we already have at least one full REQ socket, in which case we do a non-blocking poll
    let timeout = if True then (-1) else (0 :: Int64)
    zhs_poll (socketsToPoll2 preparedSockets) timeout >>= \case
      Left err -> pure (Left err)
      Right _n -> Right <$> readySockets preparedSockets

-- | milliseconds
pollFor :: Sockets -> Int -> IO (Either Error (Maybe Ready))
pollFor sockets timeout =
  pollFor_ sockets (fromIntegral @Int @Int64 timeout)

pollFor_ :: Sockets -> Int64 -> IO (Either Error (Maybe Ready))
pollFor_ sockets timeout = do
  preparedSockets <- prepareSockets sockets
  keepAlive sockets do
    zhs_poll (socketsToPoll2 preparedSockets) timeout >>= \case
      Left err -> pure (Left err)
      Right n ->
        if n == 0
          then pure (Right Nothing)
          else Right . Just <$> readySockets preparedSockets

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
