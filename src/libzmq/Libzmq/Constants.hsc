{-# LANGUAGE CPP #-}

module Libzmq.Constants where

#include <zmq.h>

import Foreign.C (CInt)


ioThreads :: CInt
maxSockets :: CInt
pub :: CInt
sub :: CInt
xpub :: CInt
xsub :: CInt

ioThreads = #const ZMQ_IO_THREADS
maxSockets = #const ZMQ_MAX_SOCKETS
pub = #const ZMQ_PUB
sub = #const ZMQ_SUB
xpub = #const ZMQ_XPUB
xsub = #const ZMQ_XSUB
