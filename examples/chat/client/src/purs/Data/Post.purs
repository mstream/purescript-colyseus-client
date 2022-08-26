module Data.Post
  ( MessagePayload
  , NotificationPayload
  , Post(..)
  , PostPayload
  ) where

import Prelude

import Data.Argonaut.Decode
  ( class DecodeJson
  , JsonDecodeError(..)
  , (.:)
  )
import Data.Argonaut.Decode as AD
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.NonEmpty (NonEmptyString)
import Data.Text (Text)
import Data.Text as Text
import Data.Timestamp (Timestamp)
import StringParser as SP

data Post
  = Message MessagePayload
  | Notification NotificationPayload

derive instance Generic Post _

derive instance Eq Post

instance Show Post where
  show = genericShow

type MessagePayload = PostPayload (author ∷ NonEmptyString)
type NotificationPayload = PostPayload ()

type PostPayload r =
  { text ∷ Text, timestamp ∷ Timestamp | r }

instance DecodeJson Post where
  decodeJson json = do
    obj ← AD.decodeJson json
    rawText ← obj .: "text"
    tag ← obj .: "tag"
    timestamp ← obj .: "timestamp"
    case SP.runParser (Text.text) rawText of
      Left parserError →
        Left $ TypeMismatch $ SP.printParserError parserError
      Right text →
        case tag of
          "message" → do
            author ← obj .: "author"
            pure $ Message { author, text, timestamp }
          "notification" →
            pure $ Notification { text, timestamp }
          _ →
            Left $ TypeMismatch $ "Unsupported tag: " <> tag

