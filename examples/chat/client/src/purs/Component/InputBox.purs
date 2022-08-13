module Component.InputBox (Output(..), Query(..), component) where

import Prelude

import Control.Monad.State (get, modify_)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (NonEmptyString)
import Data.User (User)
import Effect.Aff.Class (class MonadAff)
import Halogen (Component, HalogenM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utils (classes)

type ComponentMonad m a = HalogenM State Action () Output m a

type State =
  { sessionId ∷ NonEmptyString, draft ∷ String, users ∷ Users }

type Input = { sessionId ∷ NonEmptyString, users ∷ Users }

data Output = DraftSubmitted String | DraftUpdated

type Users = Map NonEmptyString User

data Action
  = Initialize
  | Receive Input
  | UpdateDraft String

data Query a = InsertText String a | Submit a

component ∷ ∀ m. MonadAff m ⇒ Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

initialState ∷ Input → State
initialState { sessionId, users } =
  { sessionId, draft: "", users }

render ∷ ∀ m. State → H.ComponentHTML Action () m
render state =
  HH.textarea
    [ classes
        [ Just "bg-slate-800", Just "h-full", Just "w-full" ]
    , HP.autofocus true
    , HP.placeholder "New message here..."
    , HP.value state.draft
    , HE.onValueInput UpdateDraft
    ]

handleAction ∷ ∀ m. MonadAff m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  Initialize →
    pure unit

  Receive { sessionId, users } →
    modify_ \state → state { sessionId = sessionId, users = users }

  UpdateDraft draft → do
    modify_ \state → state { draft = draft }
    H.raise DraftUpdated

handleQuery ∷ ∀ a m. Query a → ComponentMonad m (Maybe a)
handleQuery = case _ of
  InsertText s a → do
    modify_ \state → state { draft = state.draft <> s }
    H.raise DraftUpdated
    pure $ Just a

  Submit a → do
    { draft } ← get
    modify_ \state → state { draft = "" }
    H.raise $ DraftSubmitted draft
    pure $ Just a

