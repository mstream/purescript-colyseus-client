module Data.User (User(..)) where

import Prelude

import Colyseus.Client (JoinError(..))
import Colyseus.Client as Colyseus
import Colyseus.Client.Room (Room)
import Colyseus.Client.Room as Room
import Colyseus.Schema (Schema)
import Colyseus.Schema as Schema
import Control.Monad.Error.Class (try)
import Control.Monad.Except (runExceptT)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get, put)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode
  ( class DecodeJson
  , JsonDecodeError(..)
  , (.:)
  , (.:?)
  )
import Data.Argonaut.Decode as AD
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldMap)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Post (MessagePayload, NotificationPayload, Post(..))
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Timestamp (Timestamp)
import Data.Timestamp as Timestamp
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now (now)
import Foreign.Object (Object)
import Halogen (ClassName(..), HalogenM)
import Halogen as H
import Halogen.HTML (IProp, PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.Subscription as HS
import Utils (classes, getHostname, getProtocol, scrollIntoView)
import Web.DOM (Element)
import Web.DOM.Element (scrollHeight, setScrollTop)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.HTMLElement (offsetHeight, toElement)
import Web.HTML.Location (hostname, protocol)
import Web.HTML.Window (document, location)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

newtype User = User { leftAt ∷ Maybe Timestamp, name ∷ String }

instance DecodeJson User where
  decodeJson json = do
    obj ← AD.decodeJson json
    leftAt ← obj .:? "leftAt"
    name ← obj .: "name"
    pure $ User { leftAt, name }

