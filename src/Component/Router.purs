module Component.Router where

import Capability.Navigate (class Navigate)
import Capability.User (class ManageUser)
import Data.Functor
import Capability.Game.StartGame
import Component.BoardComponent (boardComponent)
import Component.WaitForGameComponent as WFGC
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
  | HandleBoard GTC.Action

type OpaqueSlot slot output = forall query. H.Slot query output slot

type ChildSlots =
  ( home :: OpaqueSlot Unit Output
  , game :: OpaqueSlot Unit Unit
  , board :: OpaqueSlot Unit GTC.Action
  , login :: OpaqueSlot Unit Unit
  , register :: OpaqueSlot Unit Unit
  , waitForGame :: OpaqueSlot Unit Unit
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
  => StartGame m
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
    HH.slot_ (Proxy :: _ "game") unit boardComponent unit
  Just Login ->
    HH.slot_ (Proxy :: _ "login") unit Login.component { redirect: false }
  Just Register ->
    HH.slot_ (Proxy :: _ "register") unit Register.component unit
  Just WaitForGame ->
    HH.slot_ (Proxy :: _ "waitForGame") unit WFGC.component unit

routerComponent
  :: forall m
   . MonadAff m
  => Navigate m
  => StartGame m
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

handleAction
    :: forall m
     . MonadAff m
    => StartGame m
    => Action
    -> H.HalogenM State Action ChildSlots Void m Unit
handleAction = case _ of
  ChangePage -> do
    state <- H.get
    let
      newState = state { route = Just Home }
    H.put newState
  HandleButton _ -> do
    _ <- startMultiGame
    _ <- trace "Hello, succeeded" \_ -> pure unit
    H.modify_ _ { route = Just WaitForGame }
  HandleBoard action -> pure unit
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