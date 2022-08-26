module Component.InputBox (Output(..), Query(..), component) where

import Prelude

import Autocompletion as Autocompletion
import Control.Monad.State (get, modify_, put)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Maybe (Maybe(..), isJust)
import Data.Search.Trie (Trie)
import Data.Search.Trie as Trie
import Data.SlidingList (Index(..), SlidingList)
import Data.SlidingList as SlidingList
import Data.String (CodePoint)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Tuple.Nested (type (/\), (/\))
import Data.User (User(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (AttrName(..), Component, HalogenM, RefLabel(..))
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Pictogram (emojiTrie)
import Utils (classes, eraseContent, getInnerText)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLElement (toElement)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent (MouseEvent)

type ComponentMonad m a = HalogenM State Action Slots Output m a

type State =
  { autocompletionClues ∷ AutocompletionClues
  , draft ∷ String
  , sessionId ∷ NonEmptyString
  , userTrie ∷ UserTrie
  }

type Input = { sessionId ∷ NonEmptyString, users ∷ Users }

data Output = DraftSubmitted String | DraftUpdated

type AutocompletionClues =
  SlidingList (NonEmptyString /\ NonEmptyString)

type Users = Map NonEmptyString User

type UserTrie = Trie CodePoint User

data Action
  = HandleClick MouseEvent
  | HandleInput Event
  | HandleKeyDown KeyboardEvent
  | Initialize
  | Receive Input

data Query a = InsertText String a | Submit a

type Slots ∷ ∀ k. Row k
type Slots = ()

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
  { autocompletionClues: SlidingList.empty
  , draft: mempty
  , sessionId
  , userTrie: buildUserTrie users
  }

render ∷ ∀ m. MonadEffect m ⇒ State → H.ComponentHTML Action Slots m
render state =
  HH.div
    [ classes
        [ Just "flex", Just "flex-col", Just "h-full", Just "w-full" ]
    ]
    [ HH.div
        [ classes
            [ Just
                if SlidingList.null state.autocompletionClues then
                  "invisible"
                else "visible"
            , Just "flex"
            , Just "flex-row"
            ]
        ]
        [ HH.div
            [ classes [ Just "opacity-50" ] ]
            [ HH.text "Tab ❯" ]
        , HH.div
            [ classes
                [ Just "flex"
                , Just "flex-row"
                , Just "h-full"
                , Just "overflow-clip"
                ]
            ]
            ( HH.fromPlainHTML
                <$> renderAutocomplitionClues state.autocompletionClues
            )
        , HH.div
            [ classes [ Just "opacity-50" ] ]
            [ HH.text "❮ Shift+Tab" ]
        ]
    , HH.div
        [ HP.attr (AttrName "contenteditable") "true"
        , HP.ref draftRefLabel
        , HE.onInput HandleInput
        , HE.onClick HandleClick
        , HE.onKeyDown HandleKeyDown
        , classes
            [ Just "border"
            , Just "h-full"
            , Just "overflow-auto"
            , Just "w-full"
            ]
        ]
        []
    ]

renderAutocomplitionClues ∷ AutocompletionClues → Array PlainHTML
renderAutocomplitionClues =
  map renderClue <<< Array.fromFoldable <<< SlidingList.toIndexedList
  where
  renderClue (index /\ (code /\ representation)) =
    HH.div
      [ classes
          [ if index == AtCursor then Just "bg-slate-800" else Nothing
          , Just "flex"
          , Just "flex-col"
          , Just "h-full"
          , Just "p-1"
          ]
      ]
      [ HH.div
          [ classes [ Just "text-center" ] ]
          [ HH.text $ NEString.toString representation ]
      , HH.div
          []
          [ HH.text $ NEString.toString code ]
      ]

handleAction ∷ ∀ m. MonadAff m ⇒ Action → ComponentMonad m Unit
handleAction = case _ of
  HandleInput _ → do
    detectAutocompletion

  HandleClick _ → do
    detectAutocompletion

  HandleKeyDown keyEvt → do
    case KE.key keyEvt of
      "Enter" → do
        if KE.shiftKey keyEvt then pure unit
        else do
          liftEffect $ preventDefault $ KE.toEvent keyEvt
          state ← get

          case SlidingList.currentItem state.autocompletionClues of
            Just (code /\ _) →
              Autocompletion.autocomplete code

            Nothing → do
              mbEl ← H.getHTMLElementRef draftRefLabel

              case mbEl of
                Just el → do
                  let
                    element = toElement el

                  innerText ← getInnerText element

                  when
                    (not String.null $ String.trim innerText)
                    do
                      H.raise $ DraftSubmitted innerText
                      liftEffect $ eraseContent element

                Nothing →
                  pure unit

        detectAutocompletion

      "Tab" → do
        let
          slide =
            if KE.shiftKey keyEvt then SlidingList.slideBackwards
            else SlidingList.slideForwards

        liftEffect $ preventDefault $ KE.toEvent keyEvt
        state ← get

        case slide state.autocompletionClues of
          Just clues →
            put state { autocompletionClues = clues }

          Nothing →
            pure unit

      _ →
        pure unit

  Initialize →
    pure unit

  Receive { sessionId, users } →
    modify_ \state → state
      { sessionId = sessionId
      , userTrie = buildUserTrie users
      }

buildUserTrie ∷ Users → UserTrie
buildUserTrie = Trie.fromFoldable <<< mapWithIndex \userId user →
  String.toCodePointArray (NEString.toString userId) /\ user

detectAutocompletion ∷ ∀ m. MonadAff m ⇒ ComponentMonad m Unit
detectAutocompletion = do
  mbInfo ← Autocompletion.autocompletionInfo

  clues ← case mbInfo of
    Just info →
      case info.prefix of
        Autocompletion.Pictogram → do
          let
            matches = Trie.query
              ( List.fromFoldable
                  $ String.toCodePointArray info.suffix
              )
              emojiTrie

          pure $ SlidingList.make $ foldl
            ( \acc (codePoints /\ symbol) →
                let
                  mbCode = NEString.fromString
                    $ String.fromCodePointArray
                    $ Array.fromFoldable codePoints
                in
                  case mbCode of
                    Just code →
                      let
                        entry =
                          (":" `NEString.prependString` code) /\
                            (NEString.singleton symbol)
                      in
                        entry : acc

                    Nothing → acc
            )
            Nil
            matches

        Autocompletion.UserReference → do
          state ← get

          let
            matches = Trie.query
              ( List.fromFoldable
                  $ String.toCodePointArray info.suffix
              )
              state.userTrie

          pure $ SlidingList.make $ foldl
            ( \acc (codePoints /\ (User { leftAt, name })) →
                let
                  mbUserId = NEString.fromString
                    $ String.fromCodePointArray
                    $ Array.fromFoldable codePoints
                in
                  case mbUserId of
                    Just userId →
                      if isJust leftAt || userId == state.sessionId then
                        acc
                      else
                        let
                          entry =
                            ("@" `NEString.prependString` userId) /\
                              name
                        in
                          entry : acc

                    Nothing →
                      acc
            )
            Nil
            matches

    Nothing →
      pure SlidingList.empty

  modify_ \state → state { autocompletionClues = clues }

handleQuery ∷ ∀ a m. Query a → ComponentMonad m (Maybe a)
handleQuery = case _ of
  InsertText draftText a → do
    modify_ \state → state { draft = state.draft <> draftText }
    H.raise DraftUpdated
    pure $ Just a

  Submit a → do
    state ← get
    put state { draft = "" }
    H.raise $ DraftSubmitted state.draft
    pure $ Just a

draftRefLabel ∷ RefLabel
draftRefLabel = RefLabel "draft"
