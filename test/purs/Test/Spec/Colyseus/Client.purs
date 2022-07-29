module Test.Spec.Colyseus.Client (spec) where

import Prelude

import Colyseus.Client (getAvailableRooms, joinOrCreate, makeClient)
import Colyseus.Client.Room as Room
import Control.Monad.Error.Class (throwError)
import Data.Argonaut.Core as A
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Class.Console (info)
import Effect.Exception (error)
import Effect.Ref as Ref
import Foreign.Object as Object
import Test.Spec (SpecT, afterAll_, beforeAll_, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Utils (startColyseusServer, stopColyseusServer)

spec ∷ SpecT Aff Unit Aff Unit
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

      room ← joinOrCreate client1 { options, roomName }

      Room.getId room `shouldSatisfy` \roomId →
        String.length roomId > 0

      Room.getSessionId room `shouldSatisfy` \sessionId →
        String.length sessionId > 0

    describe "listing available rooms" do
      let
        roomName = "test2"
        options = A.fromObject $ Object.fromFoldable
          [ "maxPlayers" /\ A.fromNumber 2.0 ]

      roomRef ← liftEffect $ Ref.new Nothing

      it "returns 0 when no room instances are present" do
        getAvailableRooms client1 { roomName } >>= \availableRooms →
          List.length availableRooms `shouldEqual` 0

      it "returns 1 after creation of one room" do
        void $ joinOrCreate client1 { options, roomName }

        getAvailableRooms client1 { roomName } >>= \availableRooms →
          List.length availableRooms `shouldEqual` 1

      it "returns 0 when the only room gets full" do
        room ← joinOrCreate client2 { options, roomName }
        liftEffect $ Ref.write (Just room) roomRef

        getAvailableRooms client1 { roomName } >>= \availableRooms →
          List.length availableRooms `shouldEqual` 0

      it "returns 1 when the room cases to be full" do
        mbRoom ← liftEffect $ Ref.read roomRef

        case mbRoom of
          Just room → do
            Room.leave room

            getAvailableRooms client1 { roomName } >>= \availableRooms →
              List.length availableRooms `shouldEqual` 1

          Nothing →
            throwError $ error "room of client2 not set"

    it "can add a message listener" do
      let
        roomName = "test3"
        options = A.fromObject $ Object.fromFoldable
          [ "pingEnabled" /\ A.fromBoolean true ]

      messageRef ← liftEffect $ Ref.new Nothing
      room ← joinOrCreate client1 { options, roomName }

      Room.addMessageListener room "ping" \msg →
        Ref.write (Just msg) messageRef

      delay $ Milliseconds 2000.0
      mbMessage ← liftEffect $ Ref.read messageRef

      (A.stringify <$> mbMessage) `shouldEqual`
        (Just $ A.stringify $ A.fromString "ping")

    it "can request a state" do
      let
        roomName = "test4"
        options = defaultOptions

      room ← joinOrCreate client1 { options, roomName }
      state ← Room.requestState room

      A.stringify state `shouldEqual`
        ( A.stringify $ A.fromObject $ Object.fromFoldable
            [ "number" /\ A.fromNumber 0.0
            , "string" /\ A.fromString ""
            , "messages" /\ A.jsonEmptyArray
            ]
        )

    it "can send a message" do
      let
        roomName = "test5"
        options = defaultOptions

      stateRef ← liftEffect $ Ref.new Nothing

      room ← joinOrCreate client1 { options, roomName }

      Room.addStateChangeListener room \state →
        Ref.write (Just state) stateRef

      Room.send room "test" $ A.fromString "message1"

      delay $ Milliseconds 200.0

      mbState ← liftEffect $ Ref.read stateRef

      (A.stringify <$> mbState) `shouldEqual`
        ( Just $ A.stringify $ A.fromObject $ Object.fromFoldable
            [ "number" /\ A.fromNumber 0.0
            , "string" /\ A.fromString ""
            , "messages" /\ A.fromArray [ A.fromString "message1" ]
            ]
        )
