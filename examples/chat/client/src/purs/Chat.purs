module Chat (component) where

import Prelude

import Colyseus.Client (JoinError(..))
import Colyseus.Client as Colyseus
import Colyseus.Client.Room (Room)
import Colyseus.Client.Room as Room
import Colyseus.Schema (Schema)
import Colyseus.Schema as Schema
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
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.FoldableWithIndex (foldMapWithIndex, foldWithIndexM)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Post (MessagePayload, NotificationPayload, Post(..))
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Text (Text(..), TextSegment(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Timestamp (Timestamp)
import Data.Timestamp as Timestamp
import Data.User (User(..))
import Effect.Aff (Aff, delay, forkAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now (now)
import Foreign.Object (Object)
import Halogen (HalogenM)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Pictogram (emojis)
import Type.Proxy (Proxy(..))
import Utils (classes, getHostname, getProtocol, scrollIntoView)
import Web.DOM.Element (scrollHeight, setScrollTop)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.HTMLElement (offsetHeight, toElement)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

data State
  = Joining String
  | Joined String JoinedState
  | SetUp JoinedState
  | FailedToJoin String

type JoinedState =
  { chatRoom ∷ ChatRoomState
  , draft ∷ String
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
  = HandleKey KeyboardEvent
  | HandleUserList UserList.Output
  | Initialize
  | ReceiveRoomMessage RoomMessageType Json
  | ReceiveRoomStateUpdate Schema
  | UpdateCurrentTime Instant
  | UpdateDraft String

data RoomMessageType = IsTyping

type Slots = (userList ∷ ∀ q. H.Slot q UserList.Output Unit)

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
initialState _ = Joining "chat"

render ∷ ∀ m. MonadAff m ⇒ State → H.ComponentHTML Action Slots m
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
    , draft
    , lastTimeTyped
    , now: mbNow
    , session
    } =
    HH.div
      [ classes [ Just "flex", Just "flex-row", Just "h-full" ] ]
      [ HH.div
          [ classes [ Just "h-full", Just "p-1", Just "w-4/5" ] ]
          [ case mbNow of
              Just now →
                renderConversationPanel
                  session.id
                  lastTimeTyped
                  chatRoomState.users
                  now
                  chatRoomState.posts
                  draft
              Nothing →
                renderLoading
          ]
      , HH.div
          [ classes [ Just "h-full", Just "w-1/5" ] ]
          [ HH.slot
              (Proxy ∷ _ "userList")
              unit
              UserList.component
              { maxUsers: chatRoomState.maxUsers
              , sessionId: session.id
              , users: chatRoomState.users
              }
              HandleUserList
          ]
      ]

  renderLoading =
    HH.text "Loading..."

  renderConversationPanel sessionId lastTimeTyped users now posts draft =
    HH.div
      [ classes [ Just "flex", Just "flex-col", Just "h-full" ] ]
      [ HH.div
          [ classes
              [ Just "flex"
              , Just "flex-col"
              , Just "h-5/6"
              , Just "overflow-x-hidden"
              , Just "overflow-y-scroll"
              , Just "p-1"
              ]
          ]
          ( ( Array.fromFoldable
                $ HH.fromPlainHTML
                    <$> renderPosts
                      sessionId
                      users
                      now
                      posts
            ) <>
              [ HH.fromPlainHTML
                  $ renderConversationPanelBottom
                      sessionId
                      now
                      users
                      lastTimeTyped
              ]
          )
      , HH.div
          [ classes [ Just "border", Just "h-1/6", Just "m-1" ] ]
          [ HH.textarea
              [ classes
                  [ Just "bg-slate-800", Just "h-full", Just "w-full" ]
              , HP.autofocus true
              , HP.placeholder "New message here..."
              , HP.value draft
              , HE.onValueInput UpdateDraft
              ]
          ]
      ]

renderConversationPanelBottom
  ∷ NonEmptyString → Instant → Users → LastTimeTyped → PlainHTML
renderConversationPanelBottom currentSessionId now users lastTimeTyped =
  HH.div
    [ classes
        [ Just "flex"
        , Just "flex-column"
        , Just "italic"
        , Just "py-1"
        , Just "text-slate-500"
        , Just "text-xs"
        ]
    ]
    [ HH.div_
        [ renderText
            currentSessionId
            users
            ( let
                typingUsers = lastTimeTyped # foldMapWithIndex
                  \sessionId timestamp →
                    if
                      sessionId == currentSessionId
                        || Timestamp.secondsAgo now timestamp > 2 then
                      Nil
                    else List.singleton sessionId
              in
                Text $ List.fromFoldable case typingUsers of
                  Nil →
                    []

                  nes : Nil →
                    [ UserReference nes
                    , PlainText
                        $ NEString.nes (Proxy ∷ _ " is typing...")
                    ]

                  ss →
                    ( Array.intersperse
                        (PlainText $ NEString.nes (Proxy ∷ _ ", "))
                        (UserReference <$> Array.fromFoldable ss)
                    )
                      <>
                        [ PlainText $ NEString.nes
                            (Proxy ∷ _ " are typing...")

                        ]
            )
        ]
    , HH.div
        [ HP.ref conversationBottomRefLabel, classes [ Just "h-4" ] ]
        []
    ]

renderPosts
  ∷ NonEmptyString → Users → Instant → List Post → List PlainHTML
renderPosts sessionId users now = map case _ of
  Message messagePayload →
    renderMessage sessionId users now messagePayload
  Notification notificationPayload →
    renderNotification sessionId users now notificationPayload

renderMessage
  ∷ NonEmptyString → Users → Instant → MessagePayload → PlainHTML
renderMessage sessionId users now { author, text, timestamp } =
  HH.div
    [ classes
        [ Just "flex", Just "flex-col", Just "p-1" ]
    ]
    [ HH.div
        [ classes
            [ Just "flex"
            , Just "flex-row"
            , Just "items-end"
            ]
        ]
        [ HH.p
            [ classes
                [ Just "font-medium"
                , Just "mr-1"
                ]
            ]
            [ renderText
                sessionId
                users
                $ Text
                $ List.fromFoldable [ UserReference author ]
            ]
        , HH.p
            [ classes
                [ Just "italic", Just "text-slate-500", Just "text-sm" ]
            ]
            [ HH.text $ Timestamp.ago now timestamp ]
        ]
    , HH.p
        [ classes [ Just "ml-1" ] ]
        [ renderText sessionId users text ]
    ]

renderNotification
  ∷ NonEmptyString → Users → Instant → NotificationPayload → PlainHTML
renderNotification sessionId users now { text, timestamp } =
  HH.div
    [ classes
        [ Just "flex"
        , Just "flex-col"
        , Just "p-1"
        , Just "text-slate-500"
        , Just "text-sm"
        ]
    ]
    [ HH.div
        [ classes [ Just "flex", Just "flex-row" ] ]
        [ HH.p
            [ classes [ Just "italic", Just "text-xs" ] ]
            [ HH.text $ Timestamp.ago now timestamp ]
        ]
    , HH.p
        [ classes [ Just "ml-1" ] ]
        [ renderText sessionId users text ]
    ]

renderText ∷ NonEmptyString → Users → Text → PlainHTML
renderText currentSessionId users (Text textSegments) =
  HH.p
    [ classes [ Just "whitespace-pre-wrap" ] ]
    (Array.fromFoldable $ renderTextSegment <$> textSegments)
  where
  renderTextSegment = case _ of
    Pictogram nes →
      HH.span_
        [ HH.text $ fromMaybe
            (":" <> NEString.toString nes)
            (Map.lookup nes emojis)
        ]

    PlainText nes →
      HH.span_ [ HH.text $ NEString.toString nes ]

    UserReference nes →
      case Map.lookup nes users of
        Just (User { name }) →
          HH.span
            [ classes
                [ if nes == currentSessionId then Just "font-semibold"
                  else Nothing
                , Just "text-sky-500"
                ]
            ]
            [ HH.text name ]

        Nothing →
          HH.span
            [ classes [ Just "text-slate-500" ] ]
            [ HH.text $ NEString.toString nes ]

handleAction
  ∷ ∀ o m. MonadAff m ⇒ Action → H.HalogenM State Action Slots o m Unit
handleAction = case _ of
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

          SetUp joinedState@{ draft, session } →
            if KE.shiftKey keyEvt then pure unit
            else
              let
                message = String.trim draft
              in
                if String.null message then pure unit
                else do
                  liftAff
                    $ Room.send
                        session.room
                        (NEString.nes (Proxy ∷ _ "post-my-message"))
                        (A.fromString draft)

                  put $ SetUp joinedState { draft = "" }

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
          SetUp joinedState →
            put $ SetUp joinedState
              { draft =
                  joinedState.draft
                    <> "@"
                    <> NEString.toString sessionId
              }

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
              , draft: ""
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

        scrollConversationWindowToBottom

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

        scrollConversationWindowToBottom

  UpdateCurrentTime ins → do
    let
      updateJoinedState joinedState =
        joinedState { now = Just ins }

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

      SetUp joinedState → do
        put $ SetUp joinedState { draft = s }

        liftAff
          $ Room.send
              joinedState.session.room
              (NEString.nes (Proxy ∷ _ "i-am-typing"))
              A.jsonNull

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

scrollConversationWindowToBottom
  ∷ ∀ state action slots output m
  . MonadEffect m
  ⇒ HalogenM state action slots output m Unit
scrollConversationWindowToBottom = do
  mbEl ← H.getHTMLElementRef conversationBottomRefLabel
  case mbEl of
    Just el → liftEffect do
      let
        hel = toElement el
      sh ← scrollHeight hel
      oh ← offsetHeight el
      let
        maxScroll = sh - oh
      setScrollTop maxScroll hel
      scrollIntoView hel

    Nothing →
      pure unit

conversationBottomRefLabel ∷ H.RefLabel
conversationBottomRefLabel = H.RefLabel "conversation-bottom"

