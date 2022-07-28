module Colyseus.Client.Room
  ( Room
  , addMessageListener
  , getId
  , getSessionId
  , getState
  , leave
  , requestState
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn1, Fn3, runFn1, runFn3)
import Effect (Effect)
import Effect.Aff (Aff)

type State = Json

getId ∷ Room → String
getId = runFn1 getIdImpl

getSessionId ∷ Room → String
getSessionId = runFn1 getSessionIdImpl

getState ∷ Room → String
getState = runFn1 getStateImpl

leave ∷ Room → Aff Unit
leave = toAffE <<< runFn1 leaveImpl

requestState ∷ Room → Aff State
requestState = toAffE <<< runFn1 requestStateImpl

addMessageListener ∷ Room → String → (Json → Effect Unit) → Aff Unit
addMessageListener room messageName listener =
  toAffE $ runFn3 addMessageListenerImpl room messageName listener

foreign import data Room ∷ Type

foreign import addMessageListenerImpl
  ∷ Fn3
      Room
      String
      (Json → Effect Unit)
      (Effect (Promise Unit))

foreign import getIdImpl
  ∷ Fn1
      Room
      String

foreign import getSessionIdImpl
  ∷ Fn1
      Room
      String

foreign import getStateImpl
  ∷ Fn1
      Room
      String

foreign import leaveImpl
  ∷ Fn1
      Room
      (Effect (Promise Unit))

foreign import requestStateImpl
  ∷ Fn1
      Room
      (Effect (Promise State))

