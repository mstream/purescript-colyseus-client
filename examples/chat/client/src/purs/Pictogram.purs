module Pictogram (Emojis, EmojiTrie, emojis, emojiTrie) where

import Prelude

import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.Search.Trie (Trie)
import Data.Search.Trie as Trie
import Data.String (CodePoint, codePointAt)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

type Emojis = Map NonEmptyString CodePoint

type EmojiTrie = Trie CodePoint CodePoint

emojis ∷ Emojis
emojis = Map.fromFoldable
  [ NEString.nes (Proxy ∷ _ "cry") /\ unsafeCodePoint "😢"
  , NEString.nes (Proxy ∷ _ "frown") /\ unsafeCodePoint "🙁"
  , NEString.nes (Proxy ∷ _ "grin") /\ unsafeCodePoint "😀"
  , NEString.nes (Proxy ∷ _ "kiss") /\ unsafeCodePoint "😗"
  , NEString.nes (Proxy ∷ _ "smile") /\ unsafeCodePoint "🙂"
  , NEString.nes (Proxy ∷ _ "wink") /\ unsafeCodePoint "😉"
  ]

emojiTrie ∷ EmojiTrie
emojiTrie = Trie.fromFoldable $ toEntry `mapWithIndex` emojis
  where
  toEntry code symbol =
    String.toCodePointArray (NEString.toString code) /\ symbol

unsafeCodePoint ∷ String → CodePoint
unsafeCodePoint s = unsafePartial $ fromJust $ codePointAt 0 s

