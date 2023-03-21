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
import Data.ByteString (ByteString)
import Data.Foldable qualified as Foldable
import Data.Functor ((<&>))
import Data.IORef (IORef, readIORef)
import Data.Int (Int64)
import Data.IntMap (IntMap)
import Data.List.NonEmpty qualified as List (NonEmpty)
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
import Zmq.Internal.Socket qualified as Socket
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

the :: CanPoll a => Socket a -> Sockets
the socket =
  Sockets [SomeSocket socket] 1

also :: CanPoll a => Socket a -> Sockets -> Sockets
also socket (Sockets sockets len) =
  Sockets (SomeSocket socket : sockets) (len + 1)

data Ready
  = Ready (forall a. Socket a -> Bool)

makeReady :: Set SomeSocket -> Ready
makeReady sockets =
  Ready ((`Set.member` sockets) . SomeSocket)

------------------------------------------------------------------------------------------------------------------------
-- Preparing sockets for polling

data PreparedSockets = PreparedSockets
  { -- The subset of the input sockets that actually need to be polled. Each is either a non-REQ socket, or a REQ socket
    -- with an empty message buffer.
    --
    -- TODO SmallArray
    socketsToPoll :: Primitive.Array SomeSocket,
    -- The same sockets as `socketsToPoll`, to pass to libzmq
    socketsToPoll2 :: StorableArray Int Zmq_pollitem,
    -- Are we polling any REQ sockets?
    anyREQs :: Bool,
    -- The subset of input sockets that don't need to be polled, because they are REQs with a full message buffer.
    readyREQs :: Set SomeSocket
  }

prepareSockets :: Sockets -> IO PreparedSockets
prepareSockets (Sockets sockets0 len) = do
  (readyREQs, sockets1, anyREQs) <- partitionSockets Set.empty [] False sockets0
  socketsToPoll2 <- MArray.newListArray (0, len - 1) (map someSocketToPollitem sockets1)
  pure
    PreparedSockets
      { socketsToPoll = Primitive.Array.arrayFromListN len sockets1,
        socketsToPoll2,
        anyREQs,
        readyREQs
      }
  where
    partitionSockets ::
      Set SomeSocket ->
      [SomeSocket] ->
      Bool ->
      [SomeSocket] ->
      IO (Set SomeSocket, [SomeSocket], Bool)
    partitionSockets readyREQs notReadyREQs anyREQs = \case
      [] -> pure (readyREQs, notReadyREQs, anyREQs)
      someSocket@(SomeSocket socket) : someSockets ->
        case extra socket of
          Socket.ReqExtra messageBuffer ->
            readIORef messageBuffer >>= \case
              Nothing -> partitionSockets readyREQs (someSocket : notReadyREQs) True someSockets
              Just _ -> partitionSockets (Set.insert someSocket readyREQs) notReadyREQs anyREQs someSockets
          _ -> partitionSockets readyREQs (someSocket : notReadyREQs) anyREQs someSockets

someSocketToPollitem :: SomeSocket -> Zmq_pollitem
someSocketToPollitem (SomeSocket Socket {zsocket}) =
  Zmq_pollitem_socket zsocket ZMQ_POLLIN

-- FIXME this is broken per req machinery
readySockets :: PreparedSockets -> IO Ready
readySockets PreparedSockets {socketsToPoll, socketsToPoll2} = do
  (lo, hi) <- MArray.getBounds socketsToPoll2
  let loop :: Set SomeSocket -> Int -> IO Ready
      loop !acc !i =
        if i > hi
          then pure (makeReady acc)
          else do
            pollitem <- Array.unsafeRead socketsToPoll2 i
            if Libzmq.Bindings.revents pollitem == 0
              then loop acc (i + 1)
              else loop (Set.insert (Primitive.Array.indexArray socketsToPoll i) acc) (i + 1)
  loop Set.empty lo

------------------------------------------------------------------------------------------------------------------------
-- Polling

poll :: Sockets -> IO (Either Error Ready)
poll sockets =
  poll_ sockets (-1) <&> \case
    Left err -> Left err
    -- This case should be impossible
    Right Nothing -> Right (Ready \_ -> False)
    Right (Just ready) -> Right ready

-- | milliseconds
pollFor :: Sockets -> Int -> IO (Either Error (Maybe Ready))
pollFor sockets timeout =
  poll_ sockets (fromIntegral @Int @Int64 timeout)

-- | monotonic time as reported by 'getMonotonicTimeNSec'
pollUntil :: Sockets -> Word64 -> IO (Either Error (Maybe Ready))
pollUntil sockets deadline = do
  now <- getMonotonicTimeNSec
  let timeout =
        if now > deadline
          then 0
          else -- safe downcast: can't overflow Int64 after dividing by 1,000,000
            fromIntegral @Word64 @Int64 ((deadline - now) `div` 1_000_000)
  poll_ sockets timeout

poll_ :: Sockets -> Int64 -> IO (Either Error (Maybe Ready))
poll_ sockets timeout0 = do
  preparedSockets <- prepareSockets sockets
  if Foldable.null (socketsToPoll preparedSockets)
    then -- If there are no sockets to poll, that means every socket was a full REQ socket, so just return them.
      pure (Right (Just (makeReady (readyREQs preparedSockets))))
    else keepAlive (socketsToPoll preparedSockets) do
      -- If we have any ready REQ sockets, do a non-blocking poll. Otherwise, respect the user-requested timeout.
      let timeout = if Set.null (readyREQs preparedSockets) then timeout0 else 0
      zhs_poll (socketsToPoll2 preparedSockets) timeout >>= \case
        Left err -> pure (Left err)
        Right n ->
          if n == 0
            then pure (Right Nothing)
            else Right . Just <$> readySockets preparedSockets

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
