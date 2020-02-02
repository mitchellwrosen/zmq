#!/bin/sh

if [ ! -z "$1" ]; then
  exec ghcid -c "cabal repl zmq:test:tests" -T ":main -p $1"
else
  exec cabal run zmq:test:tests
fi
