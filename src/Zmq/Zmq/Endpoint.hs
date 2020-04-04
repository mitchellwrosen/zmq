module Zmq.Endpoint
  ( Endpoint(..)
  , inproc
  ) where

import qualified Data.Text as Text

import Zmq.Internal
import Zmq.Prelude


inproc :: Text -> Maybe ( Endpoint 'TransportInproc )
inproc name = do
  guard ( not ( Text.null name ) )
  pure ( Inproc name )
