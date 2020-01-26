module Zmq.Internal.Tests where

import Zmq

runTests :: IO ()
runTests =
  Zmq.main do
    let endpoint = Inproc "foo"
    Right pub <- socket @'Pub
    Right sub <- socket @'Sub
    Right () <- bind pub endpoint
    Right () <- connect sub endpoint
    subscribe sub ""
    Right () <- send pub "hi"
    Right () <- disconnect sub endpoint
    Right () <- disconnect pub endpoint
    pure ()
