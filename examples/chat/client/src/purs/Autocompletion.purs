module Autocompletion
  ( AutocomletionPrefix(..)
  , autocomplete
  , autocompletionInfo
  , setSelectionTextContent
  ) where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign)
import Foreign as F

data AutocomletionPrefix = UserReference | Pictogram

derive instance Generic AutocomletionPrefix _

instance Show AutocomletionPrefix where
  show = genericShow

type AutocompletionInfo =
  { prefix ∷ AutocomletionPrefix, suffix ∷ String }

codepointToAutocompletionPrefix ∷ Map CodePoint AutocomletionPrefix
codepointToAutocompletionPrefix =
  Map.fromFoldable
    [ String.codePointFromChar '@' /\ UserReference
    , String.codePointFromChar ':' /\ Pictogram
    ]

autocompletionInfo ∷ ∀ m. MonadEffect m ⇒ m (Maybe AutocompletionInfo)
autocompletionInfo = liftEffect do
  mbString ← getSelectionTextContent
  pure $ mbString >>= String.uncons >>= \{ head, tail } →
    Map.lookup head codepointToAutocompletionPrefix <#> \prefix →
      { prefix, suffix: tail }

autocomplete ∷ ∀ m. MonadEffect m ⇒ NonEmptyString → m Unit
autocomplete nes =
  setSelectionTextContent $ NEString.toString nes <> " "

getSelectionTextContent ∷ ∀ m. MonadEffect m ⇒ m (Maybe String)
getSelectionTextContent = liftEffect do
  val ← getSelectionTextContentImpl
  result ← runExceptT $ F.readString val
  pure $ hush result

foreign import getSelectionTextContentImpl ∷ Effect Foreign

setSelectionTextContent ∷ ∀ m. MonadEffect m ⇒ String → m Unit
setSelectionTextContent = liftEffect <<< setSelectionTextContentImpl

foreign import setSelectionTextContentImpl ∷ String → Effect Unit
