module Zmq.Subscription
  ( pattern Subscribe,
    pattern Unsubscribe,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString

pattern Subscribe :: ByteString -> ByteString
pattern Subscribe topic <-
  (ByteString.uncons -> Just (1, topic))
  where
    Subscribe = ByteString.cons 1

pattern Unsubscribe :: ByteString -> ByteString
pattern Unsubscribe topic <-
  (ByteString.uncons -> Just (0, topic))
  where
    Unsubscribe = ByteString.cons 0
