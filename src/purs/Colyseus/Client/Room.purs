module Colyseus.Client.Room (Room, getId, getSessionId, getState, leave, requestState) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn1, runFn1)
import Effect (Effect)
import Effect.Aff (Aff)

type State = Json

getId :: Room -> String
getId = runFn1 getIdImpl

getSessionId :: Room -> String
getSessionId = runFn1 getSessionIdImpl

getState :: Room -> String
getState = runFn1 getStateImpl

leave :: Room -> Aff Unit
leave = toAffE <<< runFn1 leaveImpl

requestState :: Room -> Aff State
requestState = toAffE <<< runFn1 requestStateImpl

foreign import data Room :: Type

foreign import getIdImpl
  :: Fn1
       Room
       String

foreign import getSessionIdImpl
  :: Fn1
       Room
       String

foreign import getStateImpl
  :: Fn1
       Room
       String

foreign import leaveImpl
  :: Fn1
       Room
       (Effect (Promise Unit))

foreign import requestStateImpl
  :: Fn1
       Room
       (Effect (Promise State))
