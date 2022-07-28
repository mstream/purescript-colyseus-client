module Colyseus.Client
  ( Client
  , getAvailableRooms
  , joinOrCreate
  , makeClient
  ) where

import Prelude

import Colyseus.Client.Room (Room)
import Colyseus.Client.RoomAvailable (RoomAvailable)
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.List (List)
import Data.List as List
import Effect (Effect)
import Effect.Aff (Aff)

type MakeClientOpts r = { endpoint ∷ String | r }

type GetAvailableRoomsOpts r = { roomName ∷ String | r }

type JoinOrCreateOpts r = { roomName ∷ String, options ∷ Json | r }

makeClient ∷ ∀ r. MakeClientOpts r → Client
makeClient = runFn1 makeClientImpl

getAvailableRooms
  ∷ ∀ r. Client → GetAvailableRoomsOpts r → Aff (List RoomAvailable)
getAvailableRooms client opts = map List.fromFoldable
  $ toAffE
  $ runFn2 getAvailableRoomsImpl client opts

joinOrCreate ∷ ∀ r. Client → JoinOrCreateOpts r → Aff Room
joinOrCreate client opts = toAffE $ runFn2 joinOrCreateImpl client opts

foreign import data Client ∷ Type

foreign import makeClientImpl
  ∷ ∀ r
  . Fn1
      (MakeClientOpts r)
      Client

foreign import getAvailableRoomsImpl
  ∷ ∀ r
  . Fn2
      Client
      (GetAvailableRoomsOpts r)
      (Effect (Promise (Array RoomAvailable)))

foreign import joinOrCreateImpl
  ∷ ∀ r
  . Fn2
      Client
      (JoinOrCreateOpts r)
      (Effect (Promise Room))

