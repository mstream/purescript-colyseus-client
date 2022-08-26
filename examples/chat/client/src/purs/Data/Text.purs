module Data.Text (Text(..), TextSegment(..), text) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NEArray
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String as String
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import StringParser (Parser, fail)
import StringParser as SP

newtype Text = Text (List TextSegment)

derive newtype instance Show Text
derive newtype instance Eq Text
derive newtype instance Semigroup Text
derive newtype instance Monoid Text

text ∷ Parser Text
text = Text <$> SP.many textSegment

data TextSegment
  = Pictogram NonEmptyString
  | PlainText NonEmptyString
  | UserReference NonEmptyString

derive instance Generic TextSegment _
derive instance Eq TextSegment

instance Show TextSegment where
  show = genericShow

textSegment ∷ Parser TextSegment
textSegment =
  SP.try pictogram <|> SP.try userReference <|> plainText
  where
  pictogramPrefix = ':'
  userReferencePrefix = '@'

  pictogram = do
    void $ SP.char pictogramPrefix
    str ← SP.regex "[a-z0-9]+"

    case NEString.fromString str of
      Just nes →
        pure $ Pictogram nes

      Nothing →
        fail "emoticon ID must not be empty"

  plainText = do
    firstChar ← SP.anyChar
    otherChars ← SP.many
      $ SP.noneOf [ pictogramPrefix, userReferencePrefix ]

    pure
      $ PlainText
      $ NEString.fromFoldable1
      $ map String.codePointFromChar
      $ firstChar `NEArray.cons'` (Array.fromFoldable otherChars)

  userReference = do
    void $ SP.char userReferencePrefix
    str ← SP.regex "[a-zA-Z0-9]+"

    case NEString.fromString str of
      Just nes →
        pure $ UserReference nes

      Nothing →
        fail "session ID must not be empty"
