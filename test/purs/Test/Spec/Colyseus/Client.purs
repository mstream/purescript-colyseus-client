module Test.Spec.Colyseus.Client (spec) where

import Prelude

import Colyseus.Client (getAvailableRooms, joinOrCreate, makeClient)
import Colyseus.Client.Room as Room
import Data.Argonaut.Core as A
import Data.List as List
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, delay)
import Foreign.Object as Object
import Test.Spec (SpecT, afterAll_, beforeAll_, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Utils (startColyseusServer, stopColyseusServer)

spec :: SpecT Aff Unit Aff Unit
spec = beforeAll_ startColyseusServer $ afterAll_ stopColyseusServer do
  describe "Colyseus.Client" do
    let
      endpoint = "ws://localhost:2567"
      client1 = makeClient { endpoint }
      client2 = makeClient { endpoint }
      defaultOptions = A.jsonEmptyObject

    it "can connect to a server" do
      let
        roomName = "test1"
        options = defaultOptions

      room <- joinOrCreate client1 { options, roomName }

      Room.getSessionId room `shouldSatisfy` \s ->
        String.length s > 0

    describe "listing available rooms" do
      let
        roomName = "test2"
        options = A.fromObject $ Object.fromFoldable
          [ "maxPlayers" /\ A.fromNumber 2.0 ]

      it "returns 0 when no room instances are present" do
        getAvailableRooms client1 { roomName } >>= \availableRooms ->
          List.length availableRooms `shouldEqual` 0

      it "returns 1 after creation of one room" do
        void $ joinOrCreate client1 { options, roomName }

        getAvailableRooms client1 { roomName } >>= \availableRooms ->
          List.length availableRooms `shouldEqual` 1

      it "returns 0 when the only room gets full" do
        void $ joinOrCreate client2 { options, roomName }

        getAvailableRooms client1 { roomName } >>= \availableRooms ->
          List.length availableRooms `shouldEqual` 0

    it "can retrieve a state" do
      let
        roomName = "test3"
        options = defaultOptions

      room <- joinOrCreate client1 { options, roomName }
      state <- Room.getState room

      A.stringify state `shouldEqual`
        ( A.stringify $ A.fromObject $ Object.fromFoldable
            [ "number" /\ A.fromNumber 0.0
            , "string" /\ A.fromString ""
            ]
        )

