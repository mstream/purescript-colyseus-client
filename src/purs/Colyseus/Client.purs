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
import Data.Generic.Rep (class Generic)
import Data.List (List, Pattern)
import Data.List as List
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (message)

type MakeClientOpts r = { endpoint ∷ String | r }

type GetAvailableRoomsOpts r = { roomName ∷ String | r }

type JoinOpts r = { roomName ∷ String, options ∷ Json | r }

data JoinError
  = RoomNotDefined
  | RoomNotFound
  | Other String

derive instance Eq JoinError

derive instance Generic JoinError _

instance Show JoinError where
  show = genericShow

type JoinOrCreateError = String

type JoinOrCreateOpts r = { roomName ∷ String, options ∷ Json | r }

-- | Create a client instance
-- | 
-- | Example usage:
-- | ```purs
-- | let 
-- |   client = makeClient { endpoint = "ws://localhost:2567" }
-- | ```
makeClient ∷ ∀ r. MakeClientOpts r → Client
makeClient = runFn1 makeClientImpl

-- | Get available rooms
-- | 
-- | * locked and private rooms will be excluded
-- |
-- | Example usage:
-- | ```purs
-- | do 
-- |   availableRooms <- getAvailableRooms client { roomName: "room1" }
-- | ```
getAvailableRooms
  ∷ ∀ r. Client → GetAvailableRoomsOpts r → Aff (List RoomAvailable)
getAvailableRooms client opts = map List.fromFoldable
  $ toAffE
  $ runFn2 getAvailableRoomsImpl client opts

-- | Join a room
-- | 
-- | Example usage:
-- | ```purs
-- | do
-- |   roomResult <- runExceptT 
-- |     $ join client { roomName: "room1", options: A.jsonEmptyObject }
-- |   case roomResult of
-- |     Left RoomNotFound -> ...
-- |     Left (Other errorMsg) -> ...
-- |     Right room -> ...
-- | ```
join
  ∷ ∀ m r
  . MonadAff m
  ⇒ Client
  → JoinOpts r
  → ExceptT JoinError m Room
join client opts = do
  roomResult ← liftAff
    $ try
    $ toAffE
    $ runFn2 joinImpl client opts
  except case roomResult of
    Left error →
      case message error of
        "no rooms found with provided criteria" → Left RoomNotFound
        otherMessage → Left $
          if
            String.contains
              (Pattern "provided room name")
              otherMessage then
            RoomNotDefined
          else
            Other otherMessage
    Right room →
      Right room

-- | Join or create a room
-- | 
-- | Example usage:
-- | ```purs
-- | do
-- |   roomResult <- runExceptT 
-- |     $ joinOrCreate client { roomName: "room1", options: A.jsonEmptyObject }
-- |   case roomResult of
-- |     Left errorMsg -> ...
-- |     Right room -> ...
-- | ```
joinOrCreate
  ∷ ∀ m r
  . MonadAff m
  ⇒ Client
  → JoinOrCreateOpts r
  → ExceptT JoinOrCreateError m Room
joinOrCreate client opts = do
  roomResult ← liftAff
    $ try
    $ toAffE
    $ runFn2 joinOrCreateImpl client opts
  except case roomResult of
    Left error →
      Left $ message error
    Right room →
      Right room

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

