module Zmqhs.Endpoint
  ( Endpoint(..)
  ) where

import Data.Text (Text)


newtype Endpoint
  = Endpoint
  { unEndpoint :: Text }
  deriving newtype ( Eq, Ord, Show )
