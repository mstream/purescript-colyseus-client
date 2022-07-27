module Colyseus.Client.Room (Room, getSessionId, getState) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn1, runFn1)
import Effect (Effect)
import Effect.Aff (Aff)

type State = Json

getSessionId :: Room -> String
getSessionId = runFn1 getSessionIdImpl

getState :: Room -> Aff State
getState = toAffE <<< runFn1 getStateImpl

foreign import data Room :: Type

foreign import getSessionIdImpl
  :: Fn1
       Room
       String

foreign import getStateImpl
  :: Fn1
       Room
       (Effect (Promise State))
