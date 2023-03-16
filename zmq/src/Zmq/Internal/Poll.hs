module Zmq.Internal.Poll
  ( CanPoll (toPollable),
    Pollable (..),
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
import Data.ByteString (ByteString)
import Data.IORef (IORef, readIORef)
import Data.Int (Int64)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Libzmq
import Libzmq.Bindings qualified
import Zmq.Error (Error, enrichError, unexpectedError)
import Zmq.Internal.Socket (Socket)
import Zmq.Internal.Socket qualified as Socket

class Socket socket => CanPoll socket where
  toPollable :: socket -> Pollable

data Sockets
  = Sockets
      -- pollables in reverse order of how they were added (with postfix syntax), e.g.
      -- the X & also Y & also Z = [Z, Y, X]
      ![Pollable]
      -- length of pollables
      !Int

data Pollable
  = -- A REQ socket, with its message buffer (see Note [Requester message buffer])
    PollableREQ !Zmq_socket !(IORef (Maybe (List.NonEmpty ByteString)))
  | -- A non-REQ socket
    PollableNonREQ !Zmq_socket

pollableSocket :: Pollable -> Zmq_socket
pollableSocket = \case
  PollableREQ socket _ -> socket
  PollableNonREQ socket -> socket

the :: CanPoll socket => socket -> Sockets
the socket =
  Sockets [toPollable socket] 1

also :: CanPoll socket => socket -> Sockets -> Sockets
also socket (Sockets pollables len) =
  Sockets (toPollable socket : pollables) (len + 1)

-- Keep sockets alive for duration of IO action
keepingSocketsAlive :: Sockets -> IO a -> IO a
keepingSocketsAlive (Sockets pollables0 _len) action =
  go pollables0
  where
    go = \case
      [] -> action
      pollable : pollables ->
        Socket.keepingSocketAlive (pollableSocket pollable) (go pollables)

data PreparedSockets = PreparedSockets
  { -- The items that actually need to be polled. Each is either a non-REQ socket, or a REQ socket with an empty message
    -- buffer.
    pollitems :: StorableArray Int Zmq_pollitem,
    -- The indices of `pollitems` that correspond to REQ sockets
    reqPollitems :: IntSet,
    -- The indices of the original sockets that correspond to REQ sockets with a message in the buffer
    -- Why not IntSet: we want O(1) size
    fullReqSockets :: Set Int
  }

prepareSockets :: Sockets -> IO PreparedSockets
prepareSockets (Sockets pollables0 len) = do
  loop (len - 1) [] IntSet.empty Set.empty pollables0
  where
    loop :: Int -> [Zmq_pollitem] -> IntSet -> Set Int -> [Pollable] -> IO PreparedSockets
    loop !i pollitemsList !reqPollitems !fullReqSockets = \case
      [] -> do
        pollitems <- MArray.newListArray (0, len) pollitemsList
        pure PreparedSockets {pollitems, reqPollitems, fullReqSockets}
      PollableNonREQ socket : pollables -> do
        let pollitem = Zmq_pollitem_socket socket ZMQ_POLLIN
        loop (i - 1) (pollitem : pollitemsList) reqPollitems fullReqSockets pollables
      PollableREQ socket messageBuffer : pollables ->
        readIORef messageBuffer >>= \case
          Nothing -> do
            let pollitem = Zmq_pollitem_socket socket ZMQ_POLLIN
            -- FIXME `i` isn't right because we don't poll full REQ sockets
            loop (i - 1) (pollitem : pollitemsList) (IntSet.insert i reqPollitems) fullReqSockets pollables
          Just _message ->
            loop (i - 1) pollitemsList reqPollitems (Set.insert i fullReqSockets) pollables

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
  PreparedSockets {pollitems, reqPollitems, fullReqSockets} <- prepareSockets sockets
  keepingSocketsAlive sockets do
    -- Poll indefinitely, unless we already have at least one full REQ socket, in which case we do a non-blocking poll
    let timeout = if Set.null fullReqSockets then (-1) else 0
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
  PreparedSockets {pollitems, reqPollitems, fullReqSockets} <- prepareSockets sockets
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
