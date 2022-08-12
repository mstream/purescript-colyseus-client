module Pictogram (emojis) where

import Data.Map (Map)
import Data.Map as Map
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(..))

emojis ∷ Map NonEmptyString String
emojis = Map.fromFoldable
  [ NEString.nes (Proxy ∷ _ "cry") /\ "😢"
  , NEString.nes (Proxy ∷ _ "frown") /\ "🙁"
  , NEString.nes (Proxy ∷ _ "grin") /\ "😀"
  , NEString.nes (Proxy ∷ _ "kiss") /\ "😗"
  , NEString.nes (Proxy ∷ _ "smile") /\ "🙂"
  , NEString.nes (Proxy ∷ _ "wink") /\ "😉"
  ]
