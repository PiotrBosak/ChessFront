module Component.HomePage where

import Prelude
import Halogen.HTML.Events as HE
import Halogen as H
import Component.HTML.Header
import Halogen.HTML as HH
import Data.Maybe
import Effect.Aff.Class (class MonadAff)
import Data.Route
import Type.Proxy
import Component.GameTypeChooser as GTC
import Conduit.Component.HTML.Footer

data Action
    = MoveToGame
    | HandleOutput GTC.Output

type State =
  { a :: Int
  }

data Output = Clicked

type ChildSlots =
  (gameChooser :: forall query. H.Slot query GTC.Output Unit)

_gameChooser = Proxy :: Proxy "gameChooser"

homeComponent
  :: forall q o m. MonadAff m => H.Component q Unit Output m
homeComponent = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }

initialState :: forall input. input -> State
initialState _ = { a: 5 }

handleAction :: forall slots o m. MonadAff m => Action -> H.HalogenM State Action slots Output m Unit
handleAction r = case r of
  MoveToGame -> do
    H.raise Clicked
  HandleOutput _ -> do
    H.raise Clicked

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render {} =
  HH.div_
    [ header Nothing Game
    , (HH.slot _gameChooser unit GTC.gameTypeComponent unit HandleOutput)
    , footer
    ]
