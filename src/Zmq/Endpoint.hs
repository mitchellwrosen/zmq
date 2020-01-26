module Zmq.Endpoint
  ( Endpoint(..)
  , withEndpoint
  ) where

import Zmq.Internal
import Zmq.Prelude


withEndpoint
  :: Endpoint transport
  -> ( CString -> IO a )
  -> IO a
withEndpoint endpoint =
  withCString ( endpointToString endpoint )
