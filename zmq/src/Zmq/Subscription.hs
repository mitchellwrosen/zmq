module Zmq.Subscription
  ( pattern Subscribe,
    pattern Unsubscribe,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty as List (NonEmpty ((:|)))

pattern Subscribe :: ByteString -> List.NonEmpty ByteString
pattern Subscribe topic <-
  (ByteString.uncons -> Just (1, topic)) :| []
  where
    Subscribe topic = ByteString.cons 1 topic :| []

pattern Unsubscribe :: ByteString -> List.NonEmpty ByteString
pattern Unsubscribe topic <-
  (ByteString.uncons -> Just (0, topic)) :| []
  where
    Unsubscribe topic = ByteString.cons 0 topic :| []
