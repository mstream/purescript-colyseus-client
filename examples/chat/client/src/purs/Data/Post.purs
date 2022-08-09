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
import Data.Timestamp (Timestamp)

data Post
  = Message MessagePayload
  | Notification NotificationPayload

type MessagePayload = PostPayload (author ∷ String)
type NotificationPayload = PostPayload ()

type PostPayload r =
  { text ∷ String, timestamp ∷ Timestamp | r }

instance DecodeJson Post where
  decodeJson json = do
    obj ← AD.decodeJson json
    tag ← obj .: "tag"
    text ← obj .: "text"
    timestamp ← obj .: "timestamp"
    case tag of
      "message" → do
        author ← obj .: "author"
        pure $ Message { author, text, timestamp }
      "notification" →
        pure $ Notification { text, timestamp }
      _ →
        Left $ TypeMismatch $ "Unsupported tag: " <> tag

