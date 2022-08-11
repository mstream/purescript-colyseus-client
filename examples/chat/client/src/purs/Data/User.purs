module Data.User (User(..)) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, (.:), (.:?))
import Data.Argonaut.Decode as AD
import Data.Maybe (Maybe)
import Data.Timestamp (Timestamp)

newtype User = User { leftAt ∷ Maybe Timestamp, name ∷ String }

derive newtype instance Show User

instance DecodeJson User where
  decodeJson json = do
    obj ← AD.decodeJson json
    leftAt ← obj .:? "leftAt"
    name ← obj .: "name"
    pure $ User { leftAt, name }

