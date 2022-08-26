module Test.Spec.Colyseus.Client (spec) where

import Prelude

import Colyseus.Client (Client, JoinError(..))
import Colyseus.Client as Client
import Colyseus.Client.Room (Room)
import Colyseus.Client.Room as Room
import Colyseus.Schema as Schema
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Data.Argonaut.Core as A
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Int as Int
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty as NEString
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, delay)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref as Ref
import Foreign.Object as Object
import Test.Spec (SpecT, afterAll_, beforeAll_, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Utils (startColyseusServer, stopColyseusServer)
import Type.Proxy (Proxy(..))

spec ∷ SpecT Aff Unit Aff Unit
spec = beforeAll_ startColyseusServer $ afterAll_ stopColyseusServer do
  describe "Colyseus.Client" do
    let
      endpoint = "ws://localhost:2567"
      client1 = Client.makeClient { endpoint }
      client2 = Client.makeClient { endpoint }

    describe "joining a room" do
      it "fails when room is not defined" do
        roomResult ← joinRoom client1 "not-defined"

        case roomResult of
          Left joinError →
            joinError `shouldEqual` RoomNotDefined

          Right _ →
            fail "should fail to join"

    describe "listing available rooms" do
      let
        roomName = "test2"
        options = { maxPlayers: 2, pingEnabled: false }

      roomRef ← liftEffect $ Ref.new (Nothing ∷ Maybe Room)

      it "returns 0 when no room instances are present" do
        Client.getAvailableRooms client1 { roomName } >>=
          \availableRooms →
            List.length availableRooms `shouldEqual` 0

      it "returns 1 after creation of one room" do
        void $ joinOrCreateRoom client1 roomName options

        Client.getAvailableRooms client1 { roomName } >>=
          \availableRooms →
            List.length availableRooms `shouldEqual` 1

      it "returns 0 when the only room gets full" do
        room ← liftEither $ joinOrCreateRoom client2 roomName options

        liftEffect $ Ref.write (Just room) roomRef

        Client.getAvailableRooms client1 { roomName } >>=
          \availableRooms →
            List.length availableRooms `shouldEqual` 0

      it "returns 1 when the room cases to be full" do
        mbRoom ← liftEffect $ Ref.read roomRef

        case mbRoom of
          Just room → do
            Room.leave room

            Client.getAvailableRooms client1 { roomName } >>=
              \availableRooms →
                List.length availableRooms `shouldEqual` 1

          Nothing →
            throwError $ error "room of client2 not set"

    it "can add a message listener" do
      let
        roomName = "test3"
        options = { maxPlayers: 1, pingEnabled: true }
        messageName = NEString.nes (Proxy ∷ _ "ping")

      messageRef ← liftEffect $ Ref.new Nothing

      room ← liftEither $ joinOrCreateRoom client1 roomName options

      Room.addMessageListener room messageName \msg →
        Ref.write (Just msg) messageRef

      delay $ Milliseconds 2000.0
      mbMessage ← liftEffect $ Ref.read messageRef

      (A.stringify <$> mbMessage) `shouldEqual`
        (Just $ A.stringify $ A.fromString "ping")

    it "can request a state" do
      let
        roomName = "test4"
        options = { maxPlayers: 1, pingEnabled: false }

      room ← liftEither $ joinOrCreateRoom client1 roomName options
      state ← Room.requestState room

      A.stringify (Schema.toJson state) `shouldEqual`
        ( A.stringify $ A.fromObject $ Object.fromFoldable
            [ "number" /\ A.fromNumber 0.0
            , "string" /\ A.fromString ""
            , "messages" /\ A.jsonEmptyArray
            ]
        )

    it "can send a message" do
      let
        roomName = "test5"
        options = { maxPlayers: 1, pingEnabled: false }
        messageName = NEString.nes (Proxy ∷ _ "test")

      stateRef ← liftEffect $ Ref.new Nothing

      room ← liftEither $ joinOrCreateRoom client1 roomName options

      Room.addStateChangeListener room \state →
        Ref.write (Just state) stateRef

      Room.send room messageName $ A.fromString "message1"

      delay $ Milliseconds 200.0

      mbState ← liftEffect $ Ref.read stateRef

      (A.stringify <<< Schema.toJson <$> mbState) `shouldEqual`
        ( Just $ A.stringify $ A.fromObject $ Object.fromFoldable
            [ "number" /\ A.fromNumber 0.0
            , "string" /\ A.fromString ""
            , "messages" /\ A.fromArray [ A.fromString "message1" ]
            ]
        )

joinRoom
  ∷ Client
  → String
  → Aff (JoinError \/ Room)
joinRoom client roomName = runExceptT
  $ Client.join
      client
      { roomName
      , options: A.jsonEmptyObject
      }

liftEither ∷ ∀ a m. MonadThrow Error m ⇒ m (String \/ a) → m a
liftEither computation = computation >>= case _ of
  Left errorMsg →
    throwError $ error errorMsg

  Right value →
    pure value

joinOrCreateRoom
  ∷ Client
  → String
  → { maxPlayers ∷ Int, pingEnabled ∷ Boolean }
  → Aff (String \/ Room)
joinOrCreateRoom client roomName opts = runExceptT
  $ Client.joinOrCreate
      client
      { roomName
      , options: A.fromObject $ Object.fromFoldable
          [ "maxPlayers" /\ A.fromNumber
              (Int.toNumber opts.maxPlayers)
          , "pingEnabled" /\ A.fromBoolean opts.pingEnabled
          ]
      }
