{-# LANGUAGE CPP #-}

module Libzmq.Constants where

#include <zmq.h>

import Foreign.C (CInt)


dontwait :: CInt
events :: CInt
fd :: CInt
ioThreads :: CInt
ioThreadsDflt :: CInt
maxSockets :: CInt
maxSocketsDflt :: CInt
more :: CInt
pollin :: CInt
pollout :: CInt
pub :: CInt
sndmore :: CInt
sub :: CInt
subscribe :: CInt
unsubscribe :: CInt
xpub :: CInt
xsub :: CInt

dontwait = #const ZMQ_DONTWAIT
events = #const ZMQ_EVENTS
fd = #const ZMQ_FD
ioThreads = #const ZMQ_IO_THREADS
ioThreadsDflt = #const ZMQ_IO_THREADS_DFLT
maxSockets = #const ZMQ_MAX_SOCKETS
maxSocketsDflt = #const ZMQ_MAX_SOCKETS_DFLT
more = #const ZMQ_MORE
pollin = #const ZMQ_POLLIN
pollout = #const ZMQ_POLLOUT
pub = #const ZMQ_PUB
sndmore = #const ZMQ_SNDMORE
sub = #const ZMQ_SUB
subscribe = #const ZMQ_SUBSCRIBE
unsubscribe = #const ZMQ_UNSUBSCRIBE
xpub = #const ZMQ_XPUB
xsub = #const ZMQ_XSUB
