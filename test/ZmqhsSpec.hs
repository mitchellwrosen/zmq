module ZmqhsSpec
  ( spec
  ) where

import Test.Hspec
import UnliftIO

import qualified Zmqhs

spec :: Zmqhs.Context -> Spec
spec ctx = do
  describe "setSocketSubscribe" do
    it "fails on non-sub socket" do
      pub <- Zmqhs.open @IO ctx Zmqhs.Pub
      Zmqhs.setSocketSubscribe pub "" `shouldThrow`
        ( == Zmqhs.Error "zmq_setsockopt" Zmqhs.EINVAL "Invalid argument" )
