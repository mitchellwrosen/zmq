module Zmq.Internal.PublisherOptions
  ( Options (..),
    defaultOptions,
    lossy,
  )
where

-- TODO Show instance
data Options = Options
  { lossy_ :: !Bool
  }

instance Semigroup Options where
  Options lossy0 <> Options lossy1 =
    Options (lossy0 || lossy1)

defaultOptions :: Options
defaultOptions =
  Options
    { lossy_ = False
    }

lossy :: Options
lossy =
  defaultOptions {lossy_ = True}
