module Utils (classes, getHostname, getProtocol, scrollIntoView) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen (ClassName(..))
import Halogen.HTML (IProp)
import Halogen.HTML.Properties as HP
import Web.DOM (Element)
import Web.HTML (window)
import Web.HTML.Location (hostname, protocol)
import Web.HTML.Window (location)

getHostname ∷ Aff String
getHostname = liftEffect $ window >>= location >>= hostname

getProtocol ∷ Aff String
getProtocol = liftEffect $ window >>= location >>= protocol

classes ∷ ∀ i r. Array (Maybe String) → IProp (class ∷ String | r) i
classes =
  HP.classes <<< (foldMap $ maybe [] (Array.singleton <<< ClassName))

scrollIntoView ∷ Element → Effect Unit
scrollIntoView = runFn1 scrollIntoViewImpl

foreign import scrollIntoViewImpl ∷ Fn1 Element (Effect Unit)
