module Component.HomePage(homeComponent, Output) where

import Prelude
import Halogen.HTML.Events as HE
import Halogen.Store.Connect
import Halogen.Store.Select
import Halogen.Store.Monad (class MonadStore)
import Halogen as H
import Store as Store
import Component.HTML.Header
import Halogen.HTML as HH
import Data.Maybe
import Effect.Aff.Class (class MonadAff)
import Data.Route
import Type.Proxy
import Component.GameTypeChooser as GTC
import Conduit.Component.HTML.Footer
import Data.Profile (Profile)

data Action
  = MoveToGame
  | HandleOutput GTC.Action

type State =
  { currentUser :: Maybe Profile }

data Output = Clicked

type ChildSlots =
  (gameChooser :: forall query. H.Slot query GTC.Action Unit)

_gameChooser = Proxy :: Proxy "gameChooser"

homeComponent :: forall q m. MonadAff m => MonadStore Store.Action Store.Store m => H.Component q Unit Output m
homeComponent = connect (selectEq _.currentUser) $ H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
initialState { context: currentUser } = { currentUser: currentUser }

handleAction :: forall slots o m. MonadAff m => Action -> H.HalogenM State Action slots Output m Unit
handleAction r = case r of
  MoveToGame -> do
    H.raise Clicked
  HandleOutput _ -> do
    H.raise Clicked

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render { currentUser: currentUser } =
  HH.div_
    [ header  currentUser Game
    , (HH.slot _gameChooser unit GTC.gameTypeComponent unit HandleOutput)
    , footer
    ]
