module ZmqhsSpec
  ( spec
  ) where

import Test.Hspec

import qualified Zmqhs

spec :: Zmqhs.Context -> Spec
spec ctx = do
  describe "setSocketSubscribe" do
    it "allows double-subscribe" do
      Zmqhs.with @IO ctx Zmqhs.Sub \sub -> do
        Zmqhs.setSocketSubscribe sub "hi"
        Zmqhs.setSocketSubscribe sub "hi"

    it "allows bogus unsubscribe" do
      Zmqhs.with @IO ctx Zmqhs.Sub \sub ->
        Zmqhs.setSocketUnsubscribe sub "hi"

    it "fails on non-sub socket" do
      Zmqhs.with ctx Zmqhs.Pub \pub ->
        Zmqhs.setSocketSubscribe pub "" `shouldThrow`
          ( == Zmqhs.Error "zmq_setsockopt" Zmqhs.EINVAL "Invalid argument" )
