module Component.PostList (component) where

import Prelude

import Control.Monad.State (get, modify_, put)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
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
import Data.Timestamp as Timestamp
import Data.User (User(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component, ComponentHTML, HalogenM, RefLabel(..))
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pictogram (emojis)
import Type.Proxy (Proxy(..))
import Utils (ScrollInfo, classes, getScrollInfo, scrollIntoView)
import Web.DOM.Element (scrollHeight, setScrollTop)
import Web.HTML.HTMLElement (offsetHeight, toElement)

type ComponentMonad m a = ∀ o. HalogenM State Action () o m a

type State =
  { now ∷ Instant
  , posts ∷ Posts
  , scrollInfo ∷ Maybe ScrollInfo
  , sessionId ∷ NonEmptyString
  , typingUsers ∷ TypingUsers
  , users ∷ Users
  }

type Input =
  { now ∷ Instant
  , posts ∷ Posts
  , sessionId ∷ NonEmptyString
  , typingUsers ∷ TypingUsers
  , users ∷ Users
  }

type Posts = List Post
type TypingUsers = List NonEmptyString
type Users = Map NonEmptyString User

data Action
  = HandleScroll
  | Initialize
  | Receive Input
  | ScrollToBottom

component ∷ ∀ q o m. MonadAff m ⇒ Component q Input o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initialState ∷ Input → State
initialState input =
  { now: input.now
  , posts: input.posts
  , scrollInfo: Nothing
  , sessionId: input.sessionId
  , typingUsers: input.typingUsers
  , users: input.users
  }

render ∷ ∀ m. State → ComponentHTML Action () m
render state =
  let
    scrolledCloseToBottom =
      fromMaybe true $ isScrolledCloseToBottom <$> state.scrollInfo

    scrolledToBottom =
      fromMaybe true $ isScrolledToBottom <$> state.scrollInfo

    scrolledToTop =
      fromMaybe true $ isScrolledToTop <$> state.scrollInfo

  in
    HH.div
      [ classes
          [ Just "flex"
          , Just "flex-row"
          , Just "h-full"
          , Just "place-content-stretch"
          , Just "w-full"
          ]
      ]
      [ HH.div
          [ classes
              [ Just "shadow-[4px_0_5px_rgba(0,0,0,0.3)]"
              , Just "flex-none"
              , Just "h-full"
              , Just "w-px"
              ]
          ]
          []
      , HH.div
          [ classes
              [ Just "flex"
              , Just "flex-col"
              , Just "grow"
              , Just "place-content-stretch"
              , Just "w-full"
              ]
          ]
          [ HH.div
              [ classes
                  [ if scrolledToTop then Nothing
                    else Just "shadow-[0_4px_5px_rgba(0,0,0,0.3)]"
                  , Just "flex-none"
                  , Just "h-px"
                  , Just "w-full"
                  ]
              ]
              []
          , HH.div
              [ HE.onScroll $ const HandleScroll
              , HP.ref conversationRefLabel
              , classes
                  [ Just "flex"
                  , Just "flex-col"
                  , Just "flex-1"
                  , Just "min-h-0"
                  , Just "overflow-x-hidden"
                  , Just "overflow-y-scroll"
                  , Just "p-1"
                  ]
              ]
              ( postEntries
                  <>
                    [ HH.button
                        [ HE.onClick $ const ScrollToBottom
                        , classes
                            [ Just
                                if scrolledCloseToBottom then
                                  "invisible"
                                else "visible"
                            , Just "bg-slate-800"
                            , Just "bottom-0"
                            , Just "hover:opacity-100"
                            , Just "opacity-50"
                            , Just "rounded-lg"
                            , Just "shadow-lg"
                            , Just "sticky"
                            , Just "z-50"
                            ]
                        ]
                        [ HH.text "scroll to bottom" ]
                    , conversationBottom
                    ]
              )
          , HH.div
              [ classes
                  [ if scrolledToBottom then Nothing
                    else Just "shadow-[0_-4px_5px_rgba(0,0,0,0.3)]"
                  , Just "flex-none"
                  , Just "h-px"
                  , Just "w-full"
                  ]
              ]
              []
          ]
      , HH.div
          [ classes
              [ Just "shadow-[-4px_0_5px_rgba(0,0,0,0.3)]"
              , Just "flex-none"
              , Just "h-full"
              , Just "w-px"
              ]
          ]
          []
      ]

  where
  postEntries = Array.fromFoldable
    $ HH.fromPlainHTML
        <$> renderPosts
          state.sessionId
          state.users
          state.now
          state.posts

  conversationBottom =
    HH.fromPlainHTML
      $ renderConversationBottom
          state.sessionId
          state.users
          state.typingUsers

renderConversationBottom
  ∷ NonEmptyString → Users → TypingUsers → PlainHTML
renderConversationBottom currentSessionId users typingUsers =
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
            ( Text $ List.fromFoldable case typingUsers of
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
            (String.singleton <$> Map.lookup nes emojis)
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
            [ HH.text $ NEString.toString name ]

        Nothing →
          HH.span
            [ classes [ Just "text-slate-500" ] ]
            [ HH.text $ NEString.toString nes ]

handleAction ∷ ∀ m. MonadAff m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  HandleScroll →
    updateScrollInfo

  Initialize → do
    updateScrollInfo
    scrollConversationWindowToBottom

  Receive input → do
    updateScrollInfo
    { posts, scrollInfo, typingUsers } ← get

    put (initialState input)
      { scrollInfo = scrollInfo }

    case scrollInfo of
      Just info →
        when
          ( isScrolledCloseToBottom info
              &&
                ( posts /= input.posts || typingUsers /=
                    input.typingUsers
                )
          )
          scrollConversationWindowToBottom

      Nothing →
        pure unit

  ScrollToBottom →
    scrollConversationWindowToBottom

updateScrollInfo ∷ ∀ m. MonadAff m ⇒ ComponentMonad m Unit
updateScrollInfo = do
  mbEl ← H.getHTMLElementRef conversationRefLabel

  case mbEl of
    Just el → do
      scrollInfo ← liftAff $ getScrollInfo el
      modify_ \state → state { scrollInfo = Just scrollInfo }

    Nothing →
      pure unit

scrollConversationWindowToBottom
  ∷ ∀ m. MonadEffect m ⇒ ComponentMonad m Unit
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

isScrolledCloseToBottom ∷ ScrollInfo → Boolean
isScrolledCloseToBottom { clientHeight, scrollHeight, scrollTop } =
  scrollHeight <= clientHeight
    || scrollHeight - scrollTop - clientHeight <= 50.0

isScrolledToBottom ∷ ScrollInfo → Boolean
isScrolledToBottom { clientHeight, scrollHeight, scrollTop } =
  scrollHeight <= clientHeight
    || scrollHeight - scrollTop - clientHeight <= 1.0

isScrolledToTop ∷ ScrollInfo → Boolean
isScrolledToTop { scrollTop } = scrollTop <= 1.0

conversationBottomRefLabel ∷ RefLabel
conversationBottomRefLabel = RefLabel "conversation-bottom"

conversationRefLabel ∷ RefLabel
conversationRefLabel = RefLabel "conversation"
