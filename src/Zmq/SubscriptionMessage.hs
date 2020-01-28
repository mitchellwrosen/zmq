module Zmq.SubscriptionMessage
  ( SubscriptionMessage(..)
  , pattern SubscribeMessage
  , pattern UnsubscribeMessage
  , serialize
  ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.ByteString as ByteString

import Zmq.Prelude


data SubscriptionMessage
  = Unsubscribe ByteString
  | Subscribe ByteString
  deriving stock ( Eq, Ord, Show )

pattern SubscribeMessage :: ByteString -> NonEmpty ByteString
pattern SubscribeMessage prefix <- Cons 1 prefix :| []

pattern UnsubscribeMessage :: ByteString -> NonEmpty ByteString
pattern UnsubscribeMessage prefix <- Cons 0 prefix :| []

pattern Cons :: Word8 -> ByteString -> ByteString
pattern Cons w ws <- ( ByteString.uncons -> Just ( w, ws ) )

serialize :: SubscriptionMessage -> ByteString
serialize = \case
  Unsubscribe prefix -> ByteString.cons 0 prefix
  Subscribe prefix -> ByteString.cons 1 prefix
