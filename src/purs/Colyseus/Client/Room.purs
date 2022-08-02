module Colyseus.Client.Room
  ( Room
  , addMessageListener
  , addStateChangeListener
  , getId
  , getSessionId
  , getState
  , leave
  , requestState
  , send
  ) where

import Prelude

import Colyseus.Schema (Schema)
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, runFn1, runFn2, runFn3)
import Effect (Effect)
import Effect.Aff (Aff)

type State = Schema

addMessageListener ∷ Room → String → (Json → Effect Unit) → Aff Unit
addMessageListener room messageName listener =
  toAffE $ runFn3 addMessageListenerImpl room messageName listener

addStateChangeListener ∷ Room → (State → Effect Unit) → Aff Unit
addStateChangeListener room listener =
  toAffE $ runFn2 addStateChangeListenerImpl room listener

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

send ∷ Room → String → Json → Aff Unit
send room messageName message =
  toAffE $ runFn3 sendImpl room messageName message

foreign import data Room ∷ Type

foreign import addMessageListenerImpl
  ∷ Fn3
      Room
      String
      (Json → Effect Unit)
      (Effect (Promise Unit))

foreign import addStateChangeListenerImpl
  ∷ Fn2
      Room
      (State → Effect Unit)
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

foreign import sendImpl
  ∷ Fn3
      Room
      String
      Json
      (Effect (Promise Unit))
