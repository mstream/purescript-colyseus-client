module Component.UserList (Output(..), component) where

import Prelude

import Control.Monad.State (put)
import Data.Array as Array
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.User (User(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils (classes)

type State = Input

type Input = { maxUsers ∷ Int, sessionId ∷ String, users ∷ Users }

data Output = NameEditRequested | UserMentioned String

type Users = Map String User

data Action
  = EditName
  | Initialize
  | MentionUser String
  | Receive Input

component ∷ ∀ q m. MonadAff m ⇒ H.Component q Input Output m
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

render ∷ ∀ m. State → H.ComponentHTML Action () m
render state =
  let
    activeUsers =
      state.users # Map.filter \(User { leftAt }) → isNothing leftAt
  in
    HH.div
      [ classes [ Just "flex", Just "flex-col" ] ]
      [ HH.h2_
          [ HH.text
              $ "Users ("
                  <> (show $ Map.size activeUsers)
                  <> "/"
                  <> show state.maxUsers
                  <> ")"
          ]
      , HH.div
          [ classes [ Just "flex", Just "flex-col" ] ]
          ( Array.fromFoldable $ renderUsers state.sessionId activeUsers
          )
      ]
  where
  renderUsers sessionId users =
    Map.values $ renderUser sessionId `mapWithIndex` users

  renderUser currentSessionId sessionId (User { name }) =
    let
      isOwn = sessionId == currentSessionId
    in
      HH.div
        [ classes
            [ if isOwn then Just "font-semibold"
              else Nothing
            , Just "p-1"
            ]
        ]
        [ if isOwn then
            renderButton "✎" $ const EditName
          else
            renderButton "@" $ const $ MentionUser sessionId
        , HH.text name
        ]

  renderButton label handler = HH.button
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

handleAction
  ∷ ∀ m. MonadAff m ⇒ Action → H.HalogenM State Action () Output m Unit
handleAction = case _ of
  EditName →
    H.raise NameEditRequested
  Initialize →
    pure unit
  MentionUser sessionId →
    H.raise $ UserMentioned sessionId
  Receive input →
    put input
