module Zmq.Internal.Curve
  ( CurvePublicKey (..),
    CurveSecretKey (..),
    generateCurveKeys,
  )
where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Libzmq
import Zmq.Error (enrichError)

newtype CurvePublicKey
  = CurvePublicKey ByteString
  deriving newtype (Eq, Show)

newtype CurveSecretKey
  = CurveSecretKey ByteString
  deriving newtype (Eq, Show)

generateCurveKeys :: IO (CurvePublicKey, CurveSecretKey)
generateCurveKeys =
  zmq_curve_keypair >>= \case
    Left err -> throwIO (enrichError "zmq_curve_keypair" err)
    Right (publicKey85, secretKey85) -> do
      publicKey <- decode85 publicKey85
      secretKey <- decode85 secretKey85
      pure (CurvePublicKey publicKey, CurveSecretKey secretKey)
  where
    decode85 string =
      case zmq_z85_decode string of
        Left err -> throwIO (enrichError "zmq_z85_decode" err)
        Right bytes -> pure bytes
        
