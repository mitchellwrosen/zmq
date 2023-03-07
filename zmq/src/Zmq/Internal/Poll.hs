module Zmq.Internal.Poll
  ( CanPoll,
    Sockets,
    the,
    also,
    poll,
  )
where

import Control.Exception
import Data.Coerce (coerce)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Libzmq
import Zmq.Error (Error, enrichError, unexpectedError)
import Zmq.Internal.Socket (Socket (getSocket))

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

withPollitems :: [SomeSocket] -> ([Zmq_pollitem] -> IO a) -> IO a
withPollitems events0 action =
  let go acc = \case
        [] -> action acc
        SomeSocket socket0 : zevents ->
          getSocket socket0 \socket ->
            go (Zmq_pollitem_socket socket ZMQ_POLLIN : acc) zevents
   in go [] events0

poll :: Sockets -> IO (Either Error (Int -> Bool))
poll (Sockets sockets) =
  withPollitems sockets \items0 ->
    zmq_pollitems items0 \items -> do
      let loop =
            zmq_poll items (-1) >>= \case
              Left errno ->
                let err = enrichError "zmq_poll" errno
                 in case errno of
                      EINTR -> pure (Left err)
                      EFAULT -> throwIO err
                      ETERM -> pure (Left err)
                      _ -> unexpectedError err
              Right zevents ->
                let indices = f IntSet.empty (zip [0 ..] zevents)
                 in pure (Right (`IntSet.member` indices))
      loop
  where
    f :: IntSet -> [(Int, Zmq_events)] -> IntSet
    f !acc = \case
      (i, zevents) : xs ->
        let !acc1 = if zevents /= Zmq_events 0 then IntSet.insert i acc else acc
         in f acc1 xs
      [] -> acc
