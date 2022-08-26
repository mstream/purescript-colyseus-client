module Utils
  ( ScrollInfo
  , classes
  , eraseContent
  , getHostname
  , getInnerText
  , getProtocol
  , getScrollInfo
  , isUserTyping
  , scrollIntoView
  ) where

import Prelude

import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Foldable (foldMap)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe, maybe)
import Data.Timestamp (Timestamp)
import Data.Timestamp as Timestamp
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML.Properties as HP
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Location (hostname, protocol)
import Web.HTML.Window (location)

isUserTyping ∷ Instant → Timestamp → Boolean
isUserTyping now timestamp = Timestamp.secondsAgo now timestamp < 2

getHostname ∷ Aff String
getHostname = liftEffect $ window >>= location >>= hostname

getProtocol ∷ Aff String
getProtocol = liftEffect $ window >>= location >>= protocol

type ScrollInfo =
  { clientHeight ∷ Number
  , scrollHeight ∷ Number
  , scrollTop ∷ Number
  }

eraseContent ∷ ∀ m. MonadEffect m ⇒ Element → m Unit
eraseContent = liftEffect <<< runFn1 eraseContentImpl

foreign import eraseContentImpl ∷ Fn1 Element (Effect Unit)

getInnerText ∷ ∀ m. MonadEffect m ⇒ Element → m String
getInnerText = liftEffect <<< runFn1 getInnerTextImpl

foreign import getInnerTextImpl ∷ Fn1 Element (Effect String)

getScrollInfo ∷ HTMLElement → Aff ScrollInfo
getScrollInfo htmlElement = liftEffect do
  let
    element = toElement htmlElement

  clientHeight ← Element.clientHeight element
  scrollHeight ← Element.scrollHeight element
  scrollTop ← Element.scrollTop element
  pure { clientHeight, scrollHeight, scrollTop }

classes ∷ ∀ i r. Array (Maybe String) → IProp (class ∷ String | r) i
classes =
  HP.classes <<< (foldMap $ maybe [] (Array.singleton <<< ClassName))

scrollIntoView ∷ Element → Effect Unit
scrollIntoView = runFn1 scrollIntoViewImpl

foreign import scrollIntoViewImpl ∷ Fn1 Element (Effect Unit)
