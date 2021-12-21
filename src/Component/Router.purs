module Component.Router where

import Prelude
import Debug
import Data.Either (hush)
import Halogen.HTML.Events as HE
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Component.BoardComponent
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Data.Route
import Data.Profile
import Data.Maybe
import Component.HomePage

data Query a = Navigate Route a

type State =
  { route :: Maybe Route
  , currentUser :: Maybe Profile
  }

data Action
  = Initialize
  | ChangePage
  | Receive (Connected (Maybe Profile) Unit)


type OpaqueSlot slot = forall query. H.Slot query Void slot

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , game :: OpaqueSlot Unit
  )

initialState _ =
  { route : Nothing
  , currentUser : Nothing
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action ChildSlots m
render { route, currentUser }  = case route of
    Nothing ->
          HH.div_
          [  HH.text "Oh no! That page wasn't found."
          , HH.button [ HE.onClick \_ -> ChangePage ] [HH.text "click me" ]
           ]
    Just Home ->
      HH.slot_ (Proxy :: _ "home") unit homeComponent unit
    Just Game ->
      HH.slot_ (Proxy :: _ "game") unit boardComponent unit

routerComponent
  :: forall m
   . MonadAff m
   -- => Navigate m
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
    state <- H.get
    let
      newState = trace "Plumba" \_ -> state { route = Just Home }
    H.put newState
  _ -> do
    state <- H.get
    H.put state


handleQuery :: forall a m. MonadAff m => Query a -> H.HalogenM State Action ChildSlots Void m (Maybe a)
handleQuery = case _ of
  Navigate dest a -> do
    { route, currentUser } <- trace "Plumba" \_ ->  H.get
    do
      trace "Hello" \_ ->  H.modify_ _ { route = Just dest }
    pure $ Just a