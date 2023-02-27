module Zmqhs.Endpoint
  ( Endpoint(..)
  , withEndpoint
  ) where

import Data.Text (Text)
import Foreign.C.String
import qualified Data.Text as Text


newtype Endpoint
  = Endpoint
  { unEndpoint :: Text }
  deriving newtype ( Eq, Ord, Show )

withEndpoint :: Endpoint -> ( CString -> IO a ) -> IO a
withEndpoint =
  withCString . Text.unpack . unEndpoint
