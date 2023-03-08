module Zmq.Internal.Poll
  ( CanPoll,
    Sockets,
    the,
    also,
    poll,
    pollFor,
  )
where

import Control.Exception
import Data.Array.Base qualified as Array
import Data.Array.MArray qualified as MArray
import Data.Array.Storable (StorableArray)
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Libzmq
import Libzmq.Bindings qualified
import Zmq.Error (Error, enrichError, unexpectedError)
import Zmq.Internal.Socket (Socket)
import Zmq.Internal.Socket qualified as Socket

class Socket socket => CanPoll socket

newtype Sockets
  = Sockets [SomeSocket]

data SomeSocket
  = forall socket. Socket socket => SomeSocket socket

the :: CanPoll socket => socket -> Sockets
the socket =
  also socket (Sockets [])

also :: CanPoll socket => socket -> Sockets -> Sockets
also socket =
  coerce (SomeSocket socket :)

-- Keep sockets alive for duration of IO action
keepingSocketsAlive :: Sockets -> IO a -> IO a
keepingSocketsAlive (Sockets sockets0) action =
  go sockets0
  where
    go = \case
      [] -> action
      SomeSocket socket : sockets ->
        Socket.keepingSocketAlive (Socket.getSocket socket) (go sockets)

socketsArray :: Sockets -> IO (StorableArray Int Zmq_pollitem)
socketsArray (Sockets sockets0) = do
  loop 0 [] sockets0
  where
    loop :: Int -> [Zmq_pollitem] -> [SomeSocket] -> IO (StorableArray Int Zmq_pollitem)
    loop !len pollitems = \case
      [] -> MArray.newListArray (0, len) pollitems
      SomeSocket socket : sockets -> do
        let pollitem = Zmq_pollitem_socket (Socket.getSocket socket) ZMQ_POLLIN
        loop (len + 1) (pollitem : pollitems) sockets

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

poll :: Sockets -> IO (Either Error (Int -> Bool))
poll sockets =
  pollFor sockets (-1)

-- TODO make Sockets wrap the StorableArray so we don't allocate it anew each time
pollFor :: Sockets -> Int -> IO (Either Error (Int -> Bool))
pollFor sockets timeout = do
  pollitems <- socketsArray sockets
  keepingSocketsAlive sockets do
    let loop =
          zmq_poll pollitems (fromIntegral @Int @Int64 timeout) >>= \case
            Left errno ->
              let err = enrichError "zmq_poll" errno
               in case errno of
                    EINTR -> pure (Left err)
                    EFAULT -> throwIO err
                    ETERM -> pure (Left err)
                    _ -> unexpectedError err
            Right _n -> do
              indices <- socketsArrayIndices pollitems
              pure (Right (`IntSet.member` indices))
    loop
