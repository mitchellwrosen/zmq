module Zmq.Endpoint
  ( Endpoint(..)
  , withEndpoint
  , inproc
  ) where

import qualified Data.Text as Text

import Zmq.Internal
import Zmq.Prelude


withEndpoint
  :: Endpoint transport
  -> ( CString -> IO a )
  -> IO a
withEndpoint endpoint =
  withCString ( endpointToString endpoint )

inproc :: Text -> Maybe ( Endpoint 'TransportInproc )
inproc name = do
  guard ( not ( Text.null name ) )
  pure ( Inproc name )
