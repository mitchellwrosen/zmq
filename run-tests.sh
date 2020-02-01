#!/bin/sh

if [ ! -z "$1" ]; then
  exec cabal run zmq:test:tests -- -p "$1"
else
  exec cabal run zmq:test:tests
fi
