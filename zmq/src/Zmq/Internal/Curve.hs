module Zmq.Internal.Curve
  ( CurvePublicKey (..),
    CurveSecretKey (..),
    generateCurveSecretKey,
    deriveCurvePublicKey,
  )
where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Text (Text)
import Libzmq
import System.IO.Unsafe (unsafeDupablePerformIO)
import Zmq.Error (enrichError)

newtype CurvePublicKey
  = CurvePublicKey ByteString
  deriving newtype (Eq, Show)

newtype CurveSecretKey
  = CurveSecretKey ByteString
  deriving newtype (Eq, Show)

-- | Generate a CURVE secret key.
generateCurveSecretKey :: IO CurveSecretKey
generateCurveSecretKey =
  zmq_curve_keypair >>= \case
    Left err -> throwIO (enrichError "zmq_curve_keypair" err)
    Right (_publicKey85, secretKey85) -> do
      secretKey <- decode85 secretKey85
      pure (CurveSecretKey secretKey)

-- | Derive a CURVE public key from a CURVE secret key.
deriveCurvePublicKey :: CurveSecretKey -> CurvePublicKey
deriveCurvePublicKey (CurveSecretKey secretKey) =
  unsafeDupablePerformIO do
    case zmq_z85_encode secretKey of
      Left err -> throwIO (enrichError "zmq_z85_encode" err)
      Right secretKey85 ->
        case zmq_curve_public secretKey85 of
          Left err -> throwIO (enrichError "zmq_curve_public" err)
          Right publicKey85 -> coerce @(IO ByteString) @(IO CurvePublicKey) (decode85 publicKey85)

decode85 :: Text -> IO ByteString
decode85 string =
  case zmq_z85_decode string of
    Left err -> throwIO (enrichError "zmq_z85_decode" err)
    Right bytes -> pure bytes
