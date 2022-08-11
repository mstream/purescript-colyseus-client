module Data.Timestamp (Timestamp, ago) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..))
import Data.Argonaut.Decode as AD
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))

newtype Timestamp = Timestamp Instant

derive newtype instance Show Timestamp
derive newtype instance Eq Timestamp
derive newtype instance Ord Timestamp

instance DecodeJson Timestamp where
  decodeJson json = do
    x ← AD.decodeJson json
    case instant $ Milliseconds x of
      Just ins → Right $ Timestamp $ ins
      Nothing → Left $ UnexpectedValue json

ago ∷ Instant → Timestamp → String
ago now timestamp =
  units <> " ago"
  where
  units = case secondsAgo now timestamp of
    x
      | x < 120 → "seconds"
      | x < 7200 → "minutes"
      | x < 172800 → "hours"
      | otherwise → "days"

secondsAgo ∷ Instant → Timestamp → Int
secondsAgo now (Timestamp ins) =
  let
    (Milliseconds t0) = unInstant ins
    (Milliseconds t1) = unInstant now
    diff = t1 - t0
  in
    Int.round $ diff / 1000.0

