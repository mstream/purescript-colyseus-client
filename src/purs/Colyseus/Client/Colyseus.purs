module Colyseus.Client (Client, Room, RoomMessage(..), getSessionId, getState, joinOrCreate, makeClient) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)

type State = Json

type MakeClientOpts r = { endpoint :: String | r }

type JoinOrCreateOpts r = { roomName :: String | r }

type SubscribeOpts r =
  { messageCreators :: RoomMessageCreators
  , notify :: RoomMessage -> Effect Unit
  | r
  }

type RoomMessageCreators =
  { announcement :: String -> RoomMessage
  , playerJoined :: String -> RoomMessage
  }

data RoomMessage
  = Announcement String
  | PlayerJoined String

makeClient :: forall r. MakeClientOpts r -> Client
makeClient = runFn1 makeClientImpl

joinOrCreate :: forall r. Client -> JoinOrCreateOpts r -> Aff Room
joinOrCreate client opts = toAffE $ runFn2 joinOrCreateImpl client opts

getState :: Room -> Aff State
getState = toAffE <<< runFn1 getStateImpl

getSessionId :: Room -> String
getSessionId = runFn1 getSessionIdImpl

foreign import data Client :: Type
foreign import data Room :: Type

foreign import makeClientImpl
  :: forall r
   . Fn1
       (MakeClientOpts r)
       Client

foreign import joinOrCreateImpl
  :: forall r
   . Fn2
       Client
       (JoinOrCreateOpts r)
       (Effect (Promise Room))

foreign import getStateImpl
  :: Fn1
       Room
       (Effect (Promise State))

foreign import getSessionIdImpl
  :: Fn1
       Room
       String
