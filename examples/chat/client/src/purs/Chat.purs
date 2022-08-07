module Chat where

import Prelude

import Colyseus.Client (JoinError(..))
import Colyseus.Client as Colyseus
import Colyseus.Client.Room (Room)
import Colyseus.Client.Room as Room
import Colyseus.Schema (Schema)
import Colyseus.Schema as Schema
import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExceptT)
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
import Data.Either.Nested (type (\/))
import Data.Foldable (foldMap)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
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
import Web.HTML.Location (hostname)
import Web.HTML.Window (document, location)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State =
  { draft ∷ String
  , maxUsers ∷ Int
  , messages ∷ List Message
  , notifications ∷ List Notification
  , now ∷ Maybe Instant
  , session ∷ Maybe (JoinError \/ SessionState)
  , users ∷ Users
  }

type SessionState =
  { id ∷ String, room ∷ Room }

type ChatRoomState =
  { maxUsers ∷ Int
  , messages ∷ Array Message
  , notifications ∷ Array Notification
  , users ∷ Object User
  }

type Users = Map String User

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

newtype Notification =
  Notification { text ∷ String, timestamp ∷ Timestamp }

instance DecodeJson Notification where
  decodeJson json = do
    obj ← AD.decodeJson json
    text ← obj .: "text"
    timestamp ← obj .: "timestamp"
    pure $ Notification { text, timestamp }

newtype Timestamp = Timestamp Instant

derive newtype instance Show Timestamp
derive newtype instance Eq Timestamp
derive newtype instance Ord Timestamp

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
  , maxUsers: 0
  , messages: Nil
  , notifications: Nil
  , now: Nothing
  , session: Nothing
  , users: Map.empty
  }

render ∷ ∀ m. State → H.ComponentHTML Action () m
render state = HH.div
  [ classes [ Just "container", Just "mx-auto", Just "px-4" ] ]
  [ case state.session of
      Just (Left RoomNotFound) →
        HH.text "Chat room is full."

      Just (Left (Other _)) →
        HH.text "Unexpected error."

      Just (Right session) →
        HH.div
          [ classes [ Just "flex", Just "flex-row" ] ]
          [ HH.div
              [ classes [ Just "p-1", Just "w-4/5" ] ]
              [ case state.now of
                  Just now →
                    renderConversationPanel
                      now
                      state.messages
                      state.notifications
                      state.draft
                  Nothing →
                    renderLoading
              ]
          , HH.div
              [ classes [ Just "p-1", Just "w-1/5" ] ]
              [ renderUserPanel state.maxUsers session.id state.users ]
          ]

      Nothing →
        renderLoading
  ]
  where
  renderLoading =
    HH.text "Loading..."

  renderUserPanel maxUsers sessionId users =
    HH.div
      [ classes [ Just "flex", Just "flex-col" ] ]
      [ HH.h2_
          [ HH.text
              $ "Users ("
                  <> (show $ Map.size users)
                  <> "/"
                  <> show maxUsers
                  <> ")"
          ]
      , HH.div
          [ classes [ Just "flex", Just "flex-col" ] ]
          ( Array.fromFoldable
              $ HH.fromPlainHTML <$> renderUsers sessionId users
          )
      ]

  renderConversationPanel now messages notifications draft =
    HH.div
      [ classes [ Just "flex", Just "flex-col" ] ]
      [ HH.div
          [ classes [ Just "flex", Just "flex-col" ] ]
          ( Array.fromFoldable
              $ HH.fromPlainHTML
                  <$> renderMessagesAndNotifications
                    now
                    messages
                    notifications
          )
      , HH.div
          [ classes [ Just "border" ] ]
          [ HH.input
              [ classes [ Just "w-full" ]
              , HP.autofocus true
              , HP.placeholder "New message here..."
              , HP.value draft
              , HE.onValueInput UpdateDraft
              ]
          ]
      ]

renderUsers ∷ String → Users → List PlainHTML
renderUsers sessionId users =
  Map.values $ renderUser sessionId `mapWithIndex` users

renderUser ∷ String → String → User → PlainHTML
renderUser currentSessionId sessionId (User { name }) =
  HH.div
    [ classes
        [ if sessionId == currentSessionId then Just "font-semibold"
          else Nothing
        ]
    ]
    [ HH.text name ]

renderMessagesAndNotifications
  ∷ Instant → List Message → List Notification → List PlainHTML
renderMessagesAndNotifications now messages notifications =
  unfoldr f { messages, notifications }
  where
  f { messages, notifications } = case messages, notifications of
    Nil, Nil → Nothing
    message : otherMessages, Nil →
      Just
        $ renderMessage now message /\
            { messages: otherMessages, notifications: Nil }
    Nil, notification : otherNotifications →
      Just
        $ renderNotification now notification /\
            { messages: Nil, notifications: otherNotifications }
    message : otherMessages, notification : otherNotifications →
      let
        (Message { timestamp: messageTimestamp }) = message
        (Notification { timestamp: notificationTimestamp }) =
          notification
      in
        Just
          if messageTimestamp < notificationTimestamp then
            renderMessage now message /\
              { messages: otherMessages
              , notifications: notification : otherNotifications
              }
          else renderNotification now notification /\
            { messages: message : otherMessages
            , notifications: otherNotifications
            }

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

renderNotification ∷ Instant → Notification → PlainHTML
renderNotification now (Notification { text, timestamp }) =
  HH.div
    [ classes
        [ Just "flex"
        , Just "flex-col"
        , Just "p-1"
        , Just "ring-1"
        , Just "text-slate-500"
        ]
    ]
    [ HH.div
        [ classes [ Just "flex", Just "flex-row", Just "px-1" ] ]
        [ HH.p
            [ classes [ Just "italic", Just "text-sm" ] ]
            [ HH.text $ ago now timestamp ]
        ]
    , HH.p_ [ HH.text $ " " <> text ]
    ]

handleAction
  ∷ ∀ o m. MonadAff m ⇒ Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  HandleKey keyEvt →
    if KE.key keyEvt == "Enter" then do
      { draft, session } ← get
      case session of
        Just (Right { room }) → do
          liftAff $ Room.send room "message" $ A.fromString draft
          modify_ \state → state { draft = "" }
        _ →
          pure unit
    else pure unit
  Initialize → do
    roomResult ← liftAff $ try connectToRoom
    case roomResult of
      Left _ →
        modify_ \state → state
          { session = Just $ Left $ Other "unexpected error" }
      Right (Left joinError) →
        modify_ \state → state
          { session = Just $ Left joinError }
      Right (Right room) → do
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
        modify_ \state → state
          { session = Just $ Right { id: Room.getSessionId room, room }
          }
  ReceiveMessage _ → pure unit
  ReceiveRoomStateUpdate roomState →
    case AD.decodeJson $ Schema.toJson roomState of
      Left decodeError →
        liftEffect $ Console.error $
          "Could not decode the chat room state: "
            <> AD.printJsonDecodeError decodeError
      Right (chatRoomState ∷ ChatRoomState) →
        modify_ \state → state
          { maxUsers = chatRoomState.maxUsers
          , messages = List.fromFoldable
              $ chatRoomState.messages
          , notifications = List.fromFoldable
              $ chatRoomState.notifications
          , users = Map.fromFoldableWithIndex
              $ chatRoomState.users
          }
  UpdateCurrentTime ins →
    modify_ \state → state { now = Just ins }
  UpdateDraft s →
    modify_ \state → state { draft = s }

connectToRoom ∷ Aff (JoinError \/ Room)
connectToRoom = do
  host ← getHostname
  runExceptT $ Colyseus.join
    (Colyseus.makeClient { endpoint: "ws://" <> host <> ":2567" })
    { roomName: "chat", options: A.jsonEmptyObject }

getHostname ∷ Aff String
getHostname = liftEffect $ window >>= location >>= hostname

classes ∷ ∀ i r. Array (Maybe String) → IProp (class ∷ String | r) i
classes =
  HP.classes <<< (foldMap $ maybe [] (Array.singleton <<< ClassName))
