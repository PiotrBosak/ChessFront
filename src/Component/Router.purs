module Component.Router where

import Capability.Navigate (class Navigate)
import Capability.User (class ManageUser)
import Component.BoardComponent (boardComponent)
import Component.HomePage (Output, homeComponent)
import Data.Maybe (Maybe(..))
import Data.Profile (Profile)
import Data.Route (Route(..))
import Debug (trace)
import Prelude (Unit, Void, bind, discard, pure, show, unit, ($))
import Component.GameTypeChooser as GTC
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected)
import Halogen.Store.Monad (class MonadStore)
import Page.Login as Login
import Page.Register as Register
import Store as Store
import Type.Proxy (Proxy(..))

data Query a = Navigate Route a
type State =
  { route :: Maybe Route
  , currentUser :: Maybe Profile
  }

data Action
  = Initialize
  | ChangePage
  | Receive (Connected (Maybe Profile) Unit)
  | HandleButton Output
  | HandleBoard GTC.Output

type OpaqueSlot slot output = forall query. H.Slot query output slot

type ChildSlots =
  ( home :: OpaqueSlot Unit Output
  , game :: OpaqueSlot Unit Unit
  , board :: OpaqueSlot Unit GTC.Output
  , login :: OpaqueSlot Unit Unit
  , register :: OpaqueSlot Unit Unit
  )

initialState :: forall input. input -> State
initialState _ =
  { route: Nothing
  , currentUser: Nothing
  }

render
  :: forall m
   . MonadAff m
  => ManageUser m
  => Navigate m
  => MonadStore Store.Action Store.Store m
  => State
  -> H.ComponentHTML Action ChildSlots m
render { route } = case route of
  Nothing ->
    HH.slot (Proxy :: _ "home") unit homeComponent unit HandleButton
  Just Board ->
    HH.slot (Proxy :: _ "board") unit GTC.gameTypeComponent unit HandleBoard
  Just Home ->
    HH.slot (Proxy :: _ "home") unit homeComponent unit HandleButton
  Just Game ->
    HH.slot_ (Proxy :: _ "game") unit (trace "plumbulka" \_ -> boardComponent) unit
  Just Login ->
    HH.slot_ (Proxy :: _ "login") unit Login.component { redirect: false }
  Just Register ->
    HH.slot_ (Proxy :: _ "register") unit Register.component unit

routerComponent
  :: forall m
   . MonadAff m
  => Navigate m
  => ManageUser m
  => MonadStore Store.Action Store.Store m
  => H.Component Query Unit Void m
routerComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

handleAction :: forall m. MonadAff m => Action -> H.HalogenM State Action ChildSlots Void m Unit
handleAction = case _ of
  ChangePage -> do
    state <- trace "Hit change page" \_ -> H.get
    let
      newState = trace "Plumba1" \_ -> state { route = Just Home }
    H.put newState
  HandleButton _ -> do
    H.modify_ _ { route = Just Game }
  _ -> do
    state <- H.get
    H.put state

handleQuery :: forall a m. MonadAff m => Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
handleQuery = case _ of
  Navigate dest a -> do
    { route, currentUser } <- trace "Plumba2" \_ -> H.get
    do
      trace "Hello" \_ -> H.modify_ _ { route = Just dest }
    _ <- trace (show route) \_ -> pure unit
    pure $ Just a