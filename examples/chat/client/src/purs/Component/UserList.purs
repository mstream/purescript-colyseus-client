module Component.UserList (Output(..), component) where

import Prelude

import Control.Monad.State (put)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Timestamp (Timestamp)
import Data.User (User(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, ComponentHTML, HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils (classes, isUserTyping)

type ComponentMonad m a = HalogenM State Action () Output m a

type State = Input

type Input =
  { lastTimeTyped ∷ LastTimeTyped
  , maxUsers ∷ Int
  , now ∷ Instant
  , sessionId ∷ NonEmptyString
  , users ∷ Users
  }

type LastTimeTyped = Map NonEmptyString Timestamp
type Users = Map NonEmptyString User

data Output = NameEditRequested | UserMentioned NonEmptyString

data Action
  = EditName
  | Initialize
  | MentionUser NonEmptyString
  | Receive Input

component ∷ ∀ q m. MonadAff m ⇒ Component q Input Output m
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
initialState = identity

render ∷ ∀ m. State → ComponentHTML Action () m
render state =
  HH.div
    [ classes [ Just "flex", Just "flex-col" ] ]
    [ HH.h2_
        [ HH.text
            $ "Users ("
                <> (show $ Map.size state.users)
                <> "/"
                <> show state.maxUsers
                <> ")"
        ]
    , HH.div
        [ classes [ Just "flex", Just "flex-col" ] ]
        ( Array.fromFoldable
            $ Map.values
            $ renderUser `mapWithIndex` state.users
        )
    ]
  where
  renderUser sessionId (User { name }) =
    let
      isOwn = sessionId == state.sessionId

      isTyping = fromMaybe false
        $ isUserTyping state.now
            <$> Map.lookup sessionId state.lastTimeTyped
    in
      HH.div
        [ classes
            [ if isOwn then Just "font-semibold"
              else Nothing
            , Just "flex"
            , Just "flex-row"
            , Just "items-center"
            , Just "p-1"
            ]
        ]
        [ if isOwn then
            renderButton "change name" "✎"
              $ const EditName
          else
            renderButton "mention user" "@"
              $ const
              $ MentionUser sessionId
        , HH.text $ NEString.toString name
        , HH.span
            [ classes [ Just "mx-1", Just "text-slate-500" ] ]
            [ HH.text if isTyping && not isOwn then "⌨" else "" ]
        ]

  renderButton description label handler =
    HH.div
      [ classes [ Just "group", Just "h-6" ] ]
      [ HH.span
          [ classes
              [ Just "absolute"
              , Just "bg-slate-800"
              , Just "font-normal"
              , Just "group-hover:visible"
              , Just "invisible"
              , Just "mt-4"
              , Just "opacity-50"
              , Just "rounded-lg"
              , Just "shadow-lg"
              , Just "tooltip"
              , Just "z-50"
              ]
          ]
          [ HH.text description ]
      , HH.button
          [ classes
              [ Just "aspect-square"
              , Just "bg-sky-500"
              , Just "h-full"
              , Just "hover:bg-sky-400"
              , Just "mr-1"
              , Just "rounded"
              ]
          , HE.onClick handler
          , HP.type_ HP.ButtonButton
          ]
          [ HH.text label ]
      ]

handleAction ∷ ∀ m. MonadAff m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  EditName →
    H.raise NameEditRequested

  Initialize →
    pure unit

  MentionUser sessionId →
    H.raise $ UserMentioned sessionId

  Receive input →
    put input
