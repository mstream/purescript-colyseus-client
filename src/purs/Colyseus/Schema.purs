module Colyseus.Schema (Schema, toJson) where

import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn1, runFn1)

foreign import data Schema ∷ Type

toJson ∷ Schema → Json
toJson = runFn1 toJsonImpl

foreign import toJsonImpl ∷ Fn1 Schema Json

