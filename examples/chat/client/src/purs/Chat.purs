module Chat where

import Prelude

import Colyseus.Client as Colyseus
import Colyseus.Client.Room (Room)
import Colyseus.Client.Room as Room
import Colyseus.Schema (Schema)
import Colyseus.Schema as Schema
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get, modify_)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode
  ( class DecodeJson
  , JsonDecodeError(..)
  , (.:)
  )
import Data.Argonaut.Decode as AD
import Data.Array as Array
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (Aff, delay, forkAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Now (now)
import Foreign.Object (Object)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (IProp, PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State =
  { draft ∷ String
  , messages ∷ List Message
  , now ∷ Maybe Instant
  , room ∷ Maybe Room
  , users ∷ Map String User
  }

type ChatRoomState = { messages ∷ Array Message, users ∷ Object User }

newtype User = User { name ∷ String }

instance DecodeJson User where
  decodeJson json = do
    obj ← AD.decodeJson json
    name ← obj .: "name"
    pure $ User { name }

newtype Message =
  Message { author ∷ String, text ∷ String, timestamp ∷ Timestamp }

instance DecodeJson Message where
  decodeJson json = do
    obj ← AD.decodeJson json
    author ← obj .: "author"
    text ← obj .: "text"
    timestamp ← obj .: "timestamp"
    pure $ Message { author, text, timestamp }

newtype Timestamp = Timestamp Instant

derive newtype instance Show Timestamp

instance DecodeJson Timestamp where
  decodeJson json = do
    x ← AD.decodeJson json
    case instant $ Milliseconds x of
      Just ins → Right $ Timestamp $ ins
      Nothing → Left $ UnexpectedValue json

ago ∷ Instant → Timestamp → String
ago now timestamp =
  units <> " ago"
  where
  units = case secondsAgo now timestamp of
    x
      | x < 60 → "seconds"
      | x < 3600 → "minutes"
      | otherwise → "hours"

secondsAgo ∷ Instant → Timestamp → Int
secondsAgo now (Timestamp ins) =
  let
    (Milliseconds t0) = unInstant ins
    (Milliseconds t1) = unInstant now
    diff = t1 - t0
  in
    Int.round $ diff / 1000.0

data Action
  = HandleKey KeyboardEvent
  | Initialize
  | ReceiveMessage Json
  | ReceiveRoomStateUpdate Schema
  | UpdateCurrentTime Instant
  | UpdateDraft String

component ∷ ∀ q i o m. MonadAff m ⇒ H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState ∷ ∀ i. i → State
initialState _ =
  { draft: ""
  , messages: Nil
  , now: Nothing
  , room: Nothing
  , users: Map.empty
  }

render ∷ ∀ m. State → H.ComponentHTML Action () m
render state = HH.div
  [ classes [ Just "flex", Just "flex-col" ] ]
  [ HH.h1_ [ HH.text "Chat Example" ]
  , HH.div
      [ classes [ Just "flex", Just "flex-row" ] ]
      [ HH.div
          [ classes [ Just "flex", Just "flex-col" ] ]
          [ HH.div
              [ classes [ Just "flex", Just "flex-col" ] ]
              case state.now of
                Just now →
                  Array.fromFoldable $
                    HH.fromPlainHTML <<< renderMessage now
                      <$>
                        state.messages
                Nothing → [ HH.text "" ]
          , HH.div
              [ classes [ Just "border" ] ]
              [ HH.input
                  [ HP.value state.draft, HE.onValueInput UpdateDraft ]
              ]
          ]
      , HH.div
          [ classes [ Just "flex", Just "flex-col" ] ]
          [ HH.h2_ [ HH.text "Users" ]
          , HH.div
              [ classes [ Just "flex", Just "flex-col" ] ]
              ( Array.fromFoldable $ HH.fromPlainHTML <<< renderUser <$>
                  state.users
              )
          ]
      ]
  ]

renderMessage ∷ Instant → Message → PlainHTML
renderMessage now (Message { author, text, timestamp }) =
  HH.div
    [ classes
        [ Just "flex", Just "flex-col", Just "p-1", Just "ring-1" ]
    ]
    [ HH.div
        [ classes [ Just "flex", Just "flex-row", Just "px-1" ] ]
        [ HH.p
            [ classes [ Just "font-medium" ] ]
            [ HH.text author ]
        , HH.p
            [ classes [ Just "italic", Just "text-sm" ] ]
            [ HH.text $ ago now timestamp ]
        ]
    , HH.p_ [ HH.text $ " " <> text ]
    ]

renderUser ∷ User → PlainHTML
renderUser (User { name }) = HH.div_ [ HH.text name ]

handleAction
  ∷ ∀ o m. MonadAff m ⇒ Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleKey keyEvt →
    if KE.key keyEvt == "Enter" then do
      { draft, room } ← get
      case room of
        Just r → do
          liftAff $ Room.send r "message" $ A.fromString draft
          modify_ \state → state { draft = "" }
        Nothing →
          pure unit
    else pure unit
  Initialize → do
    room ← liftAff connectToRoom
    { emitter, listener } ← liftEffect HS.create
    liftAff
      $ Room.addStateChangeListener room
      $ HS.notify listener <<< ReceiveRoomStateUpdate
    void $ liftAff $ forkAff $ forever do
      delay $ Milliseconds 1000.0
      ins ← liftEffect now
      liftEffect $ HS.notify listener $ UpdateCurrentTime ins
    void $ H.subscribe emitter
    doc ← H.liftEffect $ document =<< window
    void $ H.subscribe $ eventListener
      KET.keyup
      (toEventTarget doc)
      (map HandleKey <<< KE.fromEvent)
    modify_ \state → state { room = Just room }
  ReceiveMessage msg → pure unit
  ReceiveRoomStateUpdate roomState →
    case AD.decodeJson $ Schema.toJson roomState of
      Left decodeError →
        liftEffect $ Console.error $
          "Could not decode the chat room state: "
            <> AD.printJsonDecodeError decodeError
      Right (chatRoomState ∷ ChatRoomState) →
        modify_ \state → state
          { messages = List.fromFoldable $ chatRoomState.messages
          , users = Map.fromFoldableWithIndex $ chatRoomState.users
          }
  UpdateCurrentTime ins →
    modify_ \state → state { now = Just ins }
  UpdateDraft s →
    modify_ \state → state { draft = s }

connectToRoom ∷ Aff Room
connectToRoom = Colyseus.joinOrCreate
  (Colyseus.makeClient { endpoint: "ws://localhost:2567" })
  { roomName: "chat", options: A.jsonEmptyObject }

classes ∷ ∀ i r. Array (Maybe String) → IProp (class ∷ String | r) i
classes =
  HP.classes <<< (foldMap $ maybe [] (Array.singleton <<< ClassName))
