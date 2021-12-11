module Component.HomePage where

import Prelude
import Halogen as H
import Component.HTML.Header
import Halogen.HTML as HH
import Data.Maybe
import Effect.Aff.Class (class MonadAff)
import Data.Route
data Action
  = MoveToGame


type State = {
  a :: Int
}

homeComponent
  :: forall q o m. MonadAff m => H.Component q Unit o m
homeComponent = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval {handleAction = handleAction}
  }

initialState :: forall input. input -> State
initialState _ = { a : 5 }
handleAction :: forall slots o m. MonadAff m => Action -> H.HalogenM State Action slots o m Unit
handleAction r = case r of
    MoveToGame -> do
      state <- H.get
      H.put state

render :: forall slots m. MonadAff m => State -> H.ComponentHTML Action slots m
render {} =
  HH.div_ [ header Nothing Home ]
