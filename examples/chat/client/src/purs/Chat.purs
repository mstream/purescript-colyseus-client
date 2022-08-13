module Chat (component) where

import Prelude

import Colyseus.Client (JoinError(..))
import Colyseus.Client as Colyseus
import Colyseus.Client.Room (Room)
import Colyseus.Client.Room as Room
import Colyseus.Schema (Schema)
import Colyseus.Schema as Schema
import Component.InputBox as InputBox
import Component.PostList as PostList
import Component.UserList as UserList
import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get, put)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode
  ( class DecodeJson
  , JsonDecodeError(..)
  , (.:)
  )
import Data.Argonaut.Decode as AD
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (foldMapWithIndex, foldWithIndexM)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Post (Post)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Time.Duration (Milliseconds(..))
import Data.Timestamp (Timestamp)
import Data.Timestamp as Timestamp
import Data.User (User(..))
import Effect.Aff (Aff, delay, forkAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Now (now)
import Foreign.Object (Object)
import Halogen (Component, ComponentHTML, HalogenM, Slot)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Utils (classes, getHostname, getProtocol)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type ComponentMonad m a = ∀ q. HalogenM State Action Slots q m a

data State
  = Joining String
  | Joined String JoinedState
  | SetUp JoinedState
  | FailedToJoin String

type JoinedState =
  { chatRoom ∷ ChatRoomState
  , lastTimeTyped ∷ LastTimeTyped
  , now ∷ Maybe Instant
  , session ∷ SessionState
  }

type LastTimeTyped = Map NonEmptyString Timestamp
type Users = Map NonEmptyString User

type SessionState =
  { id ∷ NonEmptyString, room ∷ Room }

newtype ChatRoomState = ChatRoomState
  { maxUsers ∷ Int
  , posts ∷ List Post
  , users ∷ Users
  }

derive newtype instance Show ChatRoomState

instance DecodeJson ChatRoomState where
  decodeJson json = do
    obj ← AD.decodeJson json
    maxUsers ← obj .: "maxUsers"
    posts ← obj .: "posts"
    users ← objectToUsers =<< obj .: "users"
    pure $ ChatRoomState { maxUsers, posts, users }
    where
    objectToUsers ∷ Object User → JsonDecodeError \/ Users
    objectToUsers = Map.empty # foldWithIndexM \k acc v →
      case NEString.fromString k of
        Just nes →
          Right $ Map.insert nes v acc
        Nothing →
          Left $ TypeMismatch "session ID must not be empty"

data Action
  = HandleInputBox InputBox.Output
  | HandleKey KeyboardEvent
  | HandleUserList UserList.Output
  | Initialize
  | ReceiveRoomMessage RoomMessageType Json
  | ReceiveRoomStateUpdate Schema
  | UpdateCurrentTime Instant
  | UpdateDraft String

data RoomMessageType = IsTyping

type Slots =
  ( inputBox ∷ Slot InputBox.Query InputBox.Output Unit
  , postList ∷ ∀ q o. Slot q o Unit
  , userList ∷ ∀ q. Slot q UserList.Output Unit
  )

component ∷ ∀ q i o m. MonadAff m ⇒ Component q i o m
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
initialState _ = Joining "chat"

render ∷ ∀ m. MonadAff m ⇒ State → ComponentHTML Action Slots m
render state = HH.div
  [ classes
      [ Just "bg-slate-900"
      , Just "h-screen"
      , Just "text-slate-50"
      , Just "w-screen"
      ]
  ]
  [ HH.div
      [ classes
          [ Just "container"
          , Just "h-full"
          , Just "mx-auto"
          ]
      ]
      [ case state of
          Joining roomName →
            renderJoiningState roomName

          Joined userNameDraft _ →
            renderJoinedState userNameDraft

          SetUp joinedState →
            renderSetUpState joinedState

          FailedToJoin reason →
            renderFailedToJoinState reason
      ]
  ]
  where
  renderJoiningState roomName =
    HH.text $ "Joining room: " <> roomName

  renderJoinedState userNameDraft =
    HH.div
      [ classes [ Just "border" ] ]
      [ HH.input
          [ classes [ Just "bg-slate-800", Just "w-full" ]
          , HP.autofocus true
          , HP.placeholder "Enter your name here..."
          , HP.value userNameDraft
          , HE.onValueInput UpdateDraft
          ]
      ]

  renderFailedToJoinState reason =
    HH.text $ "Failed to join the room: " <> reason

  renderSetUpState
    { chatRoom: (ChatRoomState chatRoomState)
    , lastTimeTyped
    , now: mbNow
    , session
    } =
    HH.div
      [ classes [ Just "flex", Just "flex-row", Just "h-full" ] ]
      case mbNow of
        Just now →
          [ HH.div
              [ classes [ Just "h-full", Just "p-1", Just "w-4/5" ] ]
              [ renderConversationPanel
                  now
                  session.id
                  lastTimeTyped
                  chatRoomState.users
                  chatRoomState.posts
              ]
          , HH.div
              [ classes [ Just "h-full", Just "w-1/5" ] ]
              [ HH.slot
                  userListPrx
                  unit
                  UserList.component
                  { lastTimeTyped
                  , maxUsers: chatRoomState.maxUsers
                  , now
                  , sessionId: session.id
                  , users: chatRoomState.users
                      # Map.filter \(User { leftAt }) →
                          isNothing leftAt
                  }
                  HandleUserList
              ]
          ]

        Nothing →
          [ renderLoading ]

  renderLoading =
    HH.text "Loading..."

  renderConversationPanel now sessionId lastTimeTyped users posts =
    HH.div
      [ classes [ Just "flex", Just "flex-col", Just "h-full" ] ]
      [ HH.div
          [ classes [ Just "h-5/6" ]
          ]
          [ HH.slot_
              postListPrx
              unit
              PostList.component
              { now
              , posts
              , sessionId
              , typingUsers: lastTimeTyped # foldMapWithIndex
                  \id timestamp →
                    if
                      id == sessionId
                        || Timestamp.secondsAgo now timestamp > 2 then
                      Nil
                    else List.singleton id
              , users
              }
          ]
      , HH.div
          [ classes [ Just "border", Just "h-1/6", Just "m-1" ] ]
          [ HH.slot
              inputBoxPrx
              unit
              InputBox.component
              { sessionId
              , users
              }
              HandleInputBox
          ]
      ]

handleAction ∷ ∀ m. MonadAff m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  HandleInputBox (InputBox.DraftSubmitted draft) → do
    state ← get

    case state of
      SetUp { session } →
        if String.null $ String.trim draft then pure unit
        else liftAff $ Room.send
          session.room
          (NEString.nes (Proxy ∷ _ "post-my-message"))
          (A.fromString draft)
      _ →
        pure unit

  HandleInputBox InputBox.DraftUpdated → do
    state ← get

    case state of
      SetUp { session } →
        liftAff
          $ Room.send
              session.room
              (NEString.nes (Proxy ∷ _ "i-am-typing"))
              A.jsonNull

      _ →
        pure unit

  HandleKey keyEvt →
    case KE.key keyEvt of
      "Enter" → do
        state ← get
        case state of
          Joined userNameDraft joinedState@{ session } →
            let
              userName = String.trim userNameDraft
            in
              if String.null userName then pure unit
              else do
                liftAff
                  $ Room.send
                      session.room
                      (NEString.nes (Proxy ∷ _ "change-my-name"))
                      (A.fromString userNameDraft)

                put $ SetUp joinedState

          SetUp _ →
            if KE.shiftKey keyEvt then pure unit
            else H.tell inputBoxPrx unit InputBox.Submit

          _ → pure unit
      _ → pure unit

  HandleUserList output →
    case output of
      UserList.NameEditRequested → do
        state ← get

        case state of
          SetUp
            joinedState@
              { chatRoom: (ChatRoomState chatRoomState), session } →
            put $ Joined
              ( fromMaybe ""
                  $ map (\(User { name }) → name)
                  $ Map.lookup session.id chatRoomState.users
              )
              joinedState

          _ →
            pure unit

      UserList.UserMentioned sessionId → do
        state ← get

        case state of
          SetUp _ →
            H.tell inputBoxPrx unit
              $ InputBox.InsertText
              $ "@" <> NEString.toString sessionId

          _ →
            pure unit

  Initialize → do
    state ← get
    case state of
      Joining roomName → do
        roomResult ← liftAff $ try $ connectToRoom roomName
        case roomResult of
          Left _ →
            put $ FailedToJoin "unexpected error"

          Right (Left RoomNotFound) →
            put $ FailedToJoin "room not found"

          Right (Left (Other errorDescription)) →
            put $ FailedToJoin errorDescription

          Right (Right room) → do
            { emitter, listener } ← liftEffect HS.create
            liftAff
              $ Room.addStateChangeListener room
              $ HS.notify listener <<< ReceiveRoomStateUpdate
            liftAff
              $ Room.addMessageListener
                  room
                  (NEString.nes (Proxy ∷ _ "is-typing"))
              $ HS.notify listener <<< ReceiveRoomMessage IsTyping
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
            put $ Joined
              ""
              { chatRoom: ChatRoomState
                  { maxUsers: 0
                  , posts: Nil
                  , users: Map.empty
                  }
              , lastTimeTyped: Map.empty
              , now: Nothing
              , session: { id: Room.getSessionId room, room }
              }
      _ → pure unit

  ReceiveRoomMessage IsTyping messagePayload → do
    case AD.decodeJson messagePayload of
      Left decodeError →
        liftEffect $ Console.error $
          "Could not decode the chat room message: "
            <> AD.printJsonDecodeError decodeError

      Right
        ( { sessionId, timestamp }
            ∷ { sessionId ∷ NonEmptyString, timestamp ∷ Timestamp }
        ) → do
        let
          updateJoinedState joinedState =
            joinedState
              { lastTimeTyped = Map.insert
                  sessionId
                  timestamp
                  joinedState.lastTimeTyped
              }

        state ← get

        case state of
          Joined userNameDraft joinedState →
            put $ Joined userNameDraft $ updateJoinedState joinedState

          SetUp joinedState →
            put $ SetUp $ updateJoinedState joinedState

          _ →
            pure unit

  ReceiveRoomStateUpdate roomState →
    case AD.decodeJson $ Schema.toJson roomState of
      Left decodeError →
        liftEffect $ Console.error $
          "Could not decode the chat room state: "
            <> AD.printJsonDecodeError decodeError

      Right (chatRoom ∷ ChatRoomState) → do
        let
          updateJoinedState joinedState =
            joinedState { chatRoom = chatRoom }

        state ← get

        case state of
          Joined userNameDraft joinedState →
            put $ Joined userNameDraft $ updateJoinedState joinedState

          SetUp joinedState →
            put $ SetUp $ updateJoinedState joinedState

          _ →
            pure unit

  UpdateCurrentTime ins → do
    let
      updateJoinedState joinedState = joinedState { now = Just ins }

    state ← get

    case state of
      Joined userNameDraft joinedState →
        put $ Joined userNameDraft $ updateJoinedState joinedState

      SetUp joinedState →
        put $ SetUp $ updateJoinedState joinedState

      _ → pure unit

  UpdateDraft s → do
    state ← get

    case state of
      Joined _ joinedState →
        put $ Joined s joinedState

      _ →
        pure unit

connectToRoom ∷ String → Aff (JoinError \/ Room)
connectToRoom roomName = do
  protocol ← getProtocol
  host ← getHostname

  let
    endpoint =
      if protocol == "https" then "wss://" <> host
      else "ws://" <> host <> ":2567"

  runExceptT $ Colyseus.join
    (Colyseus.makeClient { endpoint })
    { roomName, options: A.jsonEmptyObject }

inputBoxPrx ∷ Proxy "inputBox"
inputBoxPrx = Proxy

postListPrx ∷ Proxy "postList"
postListPrx = Proxy

userListPrx ∷ Proxy "userList"
userListPrx = Proxy
