module Zmq.Internal.Idempotent
  ( Idempotent,
    runIdempotent,
    makeIdempotent,
  )
where

import Data.Coerce
import Data.IORef

-- | An IO action that only runs once. Not thread-safe.
newtype Idempotent a
  = Idempotent (IO a)

runIdempotent :: Idempotent a -> IO a
runIdempotent =
  coerce

makeIdempotent :: IO a -> IO (Idempotent a)
makeIdempotent action = do
  resultRef <- newIORef Nothing
  pure do
    Idempotent do
      readIORef resultRef >>= \case
        Nothing -> do
          result <- action
          writeIORef resultRef (Just result)
          pure result
        Just result -> pure result
