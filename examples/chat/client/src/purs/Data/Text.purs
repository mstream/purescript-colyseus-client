module Data.Text (Text(..), TextSegment(..), text) where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NEString
import StringParser (Parser, fail)
import StringParser as SP

newtype Text = Text (List TextSegment)

derive newtype instance Show Text
derive newtype instance Eq Text

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
  pictogram <|> userReference <|> plainText
  where
  pictogram = do
    void $ SP.char ':'
    str ← SP.regex "[a-z0-9]+"

    case NEString.fromString str of
      Just nes →
        pure $ Pictogram nes

      Nothing →
        fail "emoticon ID must not be empty"

  plainText = do
    str ← SP.regex "[^:@]+"
    case NEString.fromString str of
      Just nes →
        pure $ PlainText nes

      Nothing →
        fail "text must not be empty"

  userReference = do
    void $ SP.char '@'
    str ← SP.regex "[a-zA-Z0-9]+"

    case NEString.fromString str of
      Just nes →
        pure $ UserReference nes

      Nothing →
        fail "session ID must not be empty"
