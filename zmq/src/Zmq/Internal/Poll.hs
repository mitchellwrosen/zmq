{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Zmq.Internal.Poll
  ( CanPoll,
    Sockets,
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
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
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

data P
  = forall a. P (Socket a)

the :: CanPoll a => Socket a -> Sockets
the socket =
  Sockets [P socket] 1

also :: CanPoll a => Socket a -> Sockets -> Sockets
also socket (Sockets pollables len) =
  Sockets (P socket : pollables) (len + 1)

-- Keep sockets alive for duration of IO action
keepingSocketsAlive :: Sockets -> IO a -> IO a
keepingSocketsAlive (Sockets pollables0 _len) action =
  go pollables0
  where
    go = \case
      [] -> action
      P Socket {zsocket} : pollables ->
        Socket.zhs_keepalive zsocket (go pollables)

data PreparedSockets = PreparedSockets
  { -- The items that actually need to be polled. Each is either a non-REQ socket, or a REQ socket with an empty message
    -- buffer.
    pollitems :: StorableArray Int Zmq_pollitem
  }

prepareSockets :: Sockets -> IO PreparedSockets
prepareSockets (Sockets pollables0 len) = do
  loop (len - 1) [] IntSet.empty Set.empty pollables0
  where
    loop :: Int -> [Zmq_pollitem] -> IntSet -> Set Int -> [P] -> IO PreparedSockets
    loop !i pollitemsList !reqPollitems !fullReqSockets = \case
      [] -> do
        pollitems <- MArray.newListArray (0, len) pollitemsList
        pure PreparedSockets {pollitems}
      P Socket {zsocket} : pollables -> do
        let pollitem = Zmq_pollitem_socket zsocket ZMQ_POLLIN
        loop (i - 1) (pollitem : pollitemsList) reqPollitems fullReqSockets pollables

-- Get indices that have fired
socketsArrayIndices :: StorableArray Int Zmq_pollitem -> IO IntSet
socketsArrayIndices array = do
  (lo, hi) <- MArray.getBounds array
  let loop !acc !i =
        if i > hi
          then pure acc
          else do
            pollitem <- Array.unsafeRead array i
            if Libzmq.Bindings.revents pollitem == 0
              then loop acc (i + 1)
              else loop (IntSet.insert i acc) (i + 1)
  loop IntSet.empty lo

-- TODO make Sockets wrap the StorableArray so we don't allocate it anew each time
poll :: Sockets -> IO (Either Error (Int -> Bool))
poll sockets = do
  PreparedSockets {pollitems} <- prepareSockets sockets
  keepingSocketsAlive sockets do
    -- Poll indefinitely, unless we already have at least one full REQ socket, in which case we do a non-blocking poll
    let timeout = if False then (-1) else (0 :: Int64)
    zpoll pollitems timeout >>= \case
      Left err -> pure (Left err)
      Right _n -> do
        indices <- socketsArrayIndices pollitems
        pure (Right (`IntSet.member` indices))

-- | milliseconds
pollFor :: Sockets -> Int -> IO (Either Error (Maybe (Int -> Bool)))
pollFor sockets timeout =
  pollFor_ sockets (fromIntegral @Int @Int64 timeout)

pollFor_ :: Sockets -> Int64 -> IO (Either Error (Maybe (Int -> Bool)))
pollFor_ sockets timeout = do
  PreparedSockets {pollitems} <- prepareSockets sockets
  keepingSocketsAlive sockets do
    zpoll pollitems timeout >>= \case
      Left err -> pure (Left err)
      Right n ->
        if n == 0
          then pure (Right Nothing)
          else do
            indices <- socketsArrayIndices pollitems
            pure (Right (Just (`IntSet.member` indices)))

-- | monotonic time as reported by 'getMonotonicTimeNSec'
pollUntil :: Sockets -> Word64 -> IO (Either Error (Maybe (Int -> Bool)))
pollUntil sockets deadline = do
  now <- getMonotonicTimeNSec
  let timeout =
        if now > deadline
          then 0
          else -- safe downcast: can't overflow Int64 after dividing by 1,000,000
            fromIntegral @Word64 @Int64 ((deadline - now) `div` 1_000_000)
  pollFor_ sockets timeout

zpoll :: StorableArray Int Zmq_pollitem -> Int64 -> IO (Either Error Int)
zpoll pollitems timeout = do
  zmq_poll pollitems timeout >>= \case
    Left errno ->
      let err = enrichError "zmq_poll" errno
       in case errno of
            EINTR -> pure (Left err)
            EFAULT -> throwIO err
            ETERM -> pure (Left err)
            _ -> unexpectedError err
    Right n -> pure (Right n)
