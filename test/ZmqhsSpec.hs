module ZmqhsSpec
  ( spec
  ) where

import Test.Hspec

import qualified Zmqhs

spec :: SpecWith Zmqhs.Context
spec = do
  describe "setContextMaxMessageSize" do
    -- TODO check that sends fail
    it "can be set to 0" \ctx -> do
      Zmqhs.setContextMaxMessageSize @IO ctx 0

  describe "setSocketSubscribe" do
    it "allows double-subscribe" \ctx ->
      Zmqhs.with @IO ctx Zmqhs.Sub \sub -> do
        Zmqhs.setSocketSubscribe sub "hi"
        Zmqhs.setSocketSubscribe sub "hi"

    it "fails on non-sub socket" \ctx ->
      Zmqhs.with ctx Zmqhs.Pub \pub ->
        Zmqhs.setSocketSubscribe pub "" `shouldThrow`
          ( == Zmqhs.Error "zmq_setsockopt" Zmqhs.EINVAL "Invalid argument" )

  describe "setSocketUnsubscribe" do
    it "allows bogus unsubscribe" \ctx ->
      Zmqhs.with @IO ctx Zmqhs.Sub \sub ->
        Zmqhs.setSocketUnsubscribe sub "hi"
