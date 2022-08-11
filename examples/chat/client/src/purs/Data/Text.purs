module Data.Text (Text(..), TextSegment(..), text) where

import Prelude

import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import StringParser (Parser)
import StringParser as SP

newtype Text = Text (List TextSegment)

derive newtype instance Show Text
derive newtype instance Eq Text

text ∷ Parser Text
text = Text <$> SP.many textSegment

data TextSegment = PlainText String | UserReference String

derive instance Generic TextSegment _
derive instance Eq TextSegment

instance Show TextSegment where
  show = genericShow

textSegment ∷ Parser TextSegment
textSegment =
  userReference <|> plainText
  where
  plainText = do
    str ← SP.regex "[^@]+"
    pure $ PlainText str
  userReference = do
    void $ SP.char '@'
    str ← SP.regex "[a-zA-Z0-9]+"
    pure $ UserReference str

