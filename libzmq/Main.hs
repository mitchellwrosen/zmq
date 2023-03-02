{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception
import Foreign.Marshal.Array
import Libzmq
import System.Environment
import System.Exit

main :: IO ()
main = do
  zmq do
    zmq_ctx_with \context ->
      getArgs >>= \case
        ["proxy"] -> do
          xpub <- zmq (zmq_socket context ZMQ_XPUB)
          xsub <- zmq (zmq_socket context ZMQ_XSUB)

          zmq (zmq_bind xpub "ipc:///tmp/pub.ipc")
          zmq (zmq_bind xsub "ipc:///tmp/sub.ipc")

          let pubitem = Zmq_pollitem_socket xpub ZMQ_POLLIN
              subitem = Zmq_pollitem_socket xsub mempty

          zmq_pollitems [pubitem, subitem] \pollitems -> do
            [pubevents, subevents] <- zmq (zmq_poll pollitems 1000)
            case pubevents of
              ZMQ_POLLIN -> do
                putStrLn "pub.pollin"

                zmq_msg_with \message -> do
                  _size <- zmq (zmq_msg_recv message xpub)
                  bytes <- zmq_msg_data message
                  print bytes
                  zmq (zmq_msg_close message)
              _ -> pure ()

          zmq (zmq_close xpub)
          zmq (zmq_close xsub)
          pure (Right ())
        ["pub", name] -> undefined
        ["sub", name] -> undefined
        _ -> do
          putStrLn "Usage: proxy | pub <name> | sub <name>"
          pure (Right ())

zmq :: IO (Either Zmq_error a) -> IO a
zmq action =
  action >>= \case
    Left err -> error (show err)
    Right val -> pure val
