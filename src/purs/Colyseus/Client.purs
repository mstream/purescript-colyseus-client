module Colyseus.Client
  ( Client
  , JoinError(..)
  , getAvailableRooms
  , join
  , joinOrCreate
  , makeClient
  ) where

import Prelude

import Colyseus.Client.Room (Room)
import Colyseus.Client.RoomAvailable (RoomAvailable)
import Control.Monad.Error.Class (try)
import Control.Monad.Except (ExceptT, except)
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)
import Data.List (List)
import Data.List as List
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (message)

type MakeClientOpts r = { endpoint ∷ String | r }

type GetAvailableRoomsOpts r = { roomName ∷ String | r }

type JoinOpts r = { roomName ∷ String, options ∷ Json | r }

data JoinError
  = RoomNotFound
  | Other String

type JoinOrCreateOpts r = { roomName ∷ String, options ∷ Json | r }

makeClient ∷ ∀ r. MakeClientOpts r → Client
makeClient = runFn1 makeClientImpl

getAvailableRooms
  ∷ ∀ r. Client → GetAvailableRoomsOpts r → Aff (List RoomAvailable)
getAvailableRooms client opts = map List.fromFoldable
  $ toAffE
  $ runFn2 getAvailableRoomsImpl client opts

join
  ∷ ∀ m r
  . MonadAff m
  ⇒ Client
  → JoinOpts r
  → ExceptT JoinError m Room
join client opts = do
  roomResult ← liftAff $ try $ toAffE $ runFn2 joinImpl client opts
  except case roomResult of
    Left error →
      case message error of
        "no rooms found with provided criteria" → Left RoomNotFound
        otherMessage → Left $ Other otherMessage
    Right room →
      Right room

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

foreign import joinImpl
  ∷ ∀ r
  . Fn2
      Client
      (JoinOpts r)
      (Effect (Promise Room))

foreign import joinOrCreateImpl
  ∷ ∀ r
  . Fn2
      Client
      (JoinOrCreateOpts r)
      (Effect (Promise Room))

