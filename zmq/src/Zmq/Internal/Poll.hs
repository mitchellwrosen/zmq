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
import Data.Coerce (coerce)
import Data.IORef (IORef)
import Data.Int (Int64)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Libzmq
import Libzmq.Bindings qualified
import Zmq.Error (Error, enrichError, unexpectedError)
import Zmq.Internal.Socket (Socket)
import Zmq.Internal.Socket qualified as Socket

class Socket socket => CanPoll socket where
  toPollable :: socket -> Pollable

newtype Sockets
  = Sockets [Pollable]

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
  Sockets [toPollable socket]

also :: CanPoll socket => socket -> Sockets -> Sockets
also socket =
  coerce (toPollable socket :)

-- Keep sockets alive for duration of IO action
keepingSocketsAlive :: Sockets -> IO a -> IO a
keepingSocketsAlive (Sockets pollables0) action =
  go pollables0
  where
    go = \case
      [] -> action
      pollable : pollables ->
        Socket.keepingSocketAlive (pollableSocket pollable) (go pollables)

socketsArray :: Sockets -> IO (StorableArray Int Zmq_pollitem)
socketsArray (Sockets pollables0) = do
  loop 0 [] pollables0
  where
    loop :: Int -> [Zmq_pollitem] -> [Pollable] -> IO (StorableArray Int Zmq_pollitem)
    loop !len pollitems = \case
      [] -> MArray.newListArray (0, len) pollitems
      pollable : pollables -> do
        let pollitem = Zmq_pollitem_socket (pollableSocket pollable) ZMQ_POLLIN
        loop (len + 1) (pollitem : pollitems) pollables

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
  pollitems <- socketsArray sockets
  keepingSocketsAlive sockets do
    zpoll pollitems (-1) >>= \case
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
  pollitems <- socketsArray sockets
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
          -- safe downcast: can't overflow Int64 after dividing by 1,000,000
          else fromIntegral @Word64 @Int64 ((deadline - now) `div` 1_000_000)
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
