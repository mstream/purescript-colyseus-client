module Chat where

import Prelude

import Colyseus.Client as Colyseus
import Colyseus.Client.Room (Room)
import Colyseus.Client.Room as Room
import Control.Monad.State (modify_)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as A
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS

type State = { messages ∷ List String, room ∷ Maybe Room }

data Action
  = Initialize
  | ReceiveMessage Json

component ∷ ∀ q i o m. MonadAff m ⇒ H.Component q i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState ∷ ∀ i. i → State
initialState _ = { messages: Nil, room: Nothing }

render ∷ ∀ m. State → H.ComponentHTML Action () m
render state = HH.div
  [ classes [ Just "flex", Just "flex-col" ] ]
  [ HH.text "Chat Example"
  , HH.div
      [ classes [ Just "flex", Just "flex-col" ] ]
      ( Array.fromFoldable $
          HH.div_ <<< Array.singleton <<< HH.text
            <$> state.messages
      )
  ]

handleAction
  ∷ ∀ o m. MonadAff m ⇒ Action → H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize → do
    room ← liftAff connectToRoom
    { emitter, listener } ← liftEffect HS.create
    liftAff
      $ Room.addMessageListener room "ping"
      $ HS.notify listener <<< ReceiveMessage
    void $ H.subscribe emitter
    modify_ \state → state { room = Just room }
  ReceiveMessage msg → do
    modify_ \state → state
      { messages = A.stringify msg : state.messages }

connectToRoom ∷ Aff Room
connectToRoom = Colyseus.joinOrCreate
  (Colyseus.makeClient { endpoint: "ws://localhost:2567" })
  { roomName: "chat", options: A.jsonEmptyObject }

classes ∷ ∀ i r. Array (Maybe String) → IProp (class ∷ String | r) i
classes =
  HP.classes <<< (foldMap $ maybe [] (Array.singleton <<< ClassName))
