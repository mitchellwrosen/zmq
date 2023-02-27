module Zmqhs.Endpoint
  ( Endpoint (..),
    withEndpoint,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Foreign.C.String

newtype Endpoint = Endpoint
  {unEndpoint :: Text}
  deriving newtype (Eq, Ord, Show)

withEndpoint :: Endpoint -> (CString -> IO a) -> IO a
withEndpoint =
  withCString . Text.unpack . unEndpoint
