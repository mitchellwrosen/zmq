{-# LANGUAGE CPP #-}

module Libzmq.Constants where

#include <zmq.h>

import Foreign.C (CInt)

zMQ_PUB, zMQ_SUB, zMQ_XPUB, zMQ_XSUB :: CInt
zMQ_PUB  = #const ZMQ_PUB
zMQ_SUB  = #const ZMQ_SUB
zMQ_XPUB = #const ZMQ_XPUB
zMQ_XSUB = #const ZMQ_XSUB
