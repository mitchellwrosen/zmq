#!/bin/sh

cabal run zmq:test:tests -- -p "$1"
