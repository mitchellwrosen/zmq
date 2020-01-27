module Zmq.Internal.Tests where

import Data.List.NonEmpty (NonEmpty((:|)))
import Zmq

runTests :: IO ()
runTests =
  Zmq.main do
    let endpoint = Inproc "foo"
    Right pub <- pubSocket
    Right sub <- subSocket
    Right () <- bind pub endpoint
    Right () <- connect sub endpoint
    subscribe sub ""
    Right () <- send pub "hi"
    "hi" :| [] <- recv sub
    Right () <- disconnect sub endpoint
    Right () <- disconnect pub endpoint
    pure ()
