module Zmq.Endpoint
  ( Endpoint (..),
    inproc,
  )
where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as Text
import Zmq.Internal

inproc :: Text -> Maybe (Endpoint 'TransportInproc)
inproc name = do
  guard (not (Text.null name))
  pure (Inproc name)
