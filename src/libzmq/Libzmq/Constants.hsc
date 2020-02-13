{-# LANGUAGE CPP #-}

module Libzmq.Constants where

#include <zmq.h>

import Foreign.C (CInt)


events :: CInt
fd :: CInt
ioThreads :: CInt
maxSockets :: CInt
pub :: CInt
sub :: CInt
subscribe :: CInt
xpub :: CInt
xsub :: CInt

events = #const ZMQ_EVENTS
fd = #const ZMQ_FD
ioThreads = #const ZMQ_IO_THREADS
maxSockets = #const ZMQ_MAX_SOCKETS
pub = #const ZMQ_PUB
sub = #const ZMQ_SUB
subscribe = #const ZMQ_SUBSCRIBE
xpub = #const ZMQ_XPUB
xsub = #const ZMQ_XSUB
