module Test.Spec.Colyseus.Client (spec) where

import Prelude

import Data.Tuple.Nested ((/\))
import Foreign.Object as Object
import Data.Argonaut.Core as A
import Colyseus.Client (getSessionId, getState, joinOrCreate, makeClient)
import Data.String as String
import Effect.Aff (Aff)
import Test.Spec (SpecT, afterAll_, beforeAll_, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Utils (startColyseusServer, stopColyseusServer)

spec :: SpecT Aff Unit Aff Unit
spec = beforeAll_ startColyseusServer $ afterAll_ stopColyseusServer do
  describe "Colyseus.Client" do
    let
      client = makeClient { endpoint: "ws://localhost:2567" }

    it "can connect to a server" do
      room <- joinOrCreate client { roomName: "test" }
      getSessionId room `shouldSatisfy` \s -> String.length s > 0

    it "can retrieve a state" do
      room <- joinOrCreate client { roomName: "test" }
      state <- getState room

      A.stringify state `shouldEqual`
        ( A.stringify $ A.fromObject $ Object.fromFoldable
            [ "number" /\ A.fromNumber 0.0
            , "string" /\ A.fromString ""
            ]
        )

