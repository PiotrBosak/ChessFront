module Component.WaitForGameComponent where

import Prelude
import Component.HTML.Board as B
import Control.Monad.Rec.Class (forever)
import Halogen.Subscription as HS
import Debug
import Component.HTML.Header
import Conduit.Component.HTML.Footer
import Halogen.Store.Connect
import Halogen.Store.Select
import Halogen.Query.HalogenM
import Halogen.Store.Monad (class MonadStore)
import Component.HTML.Utils (css)
import Effect.Aff.Class
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Halogen.HTML.Events as HE
import Halogen.Store.Connect
import Halogen.Store.Select
import Halogen as H
import Store as Store
import Capability.Game.StartGame
import Data.StartGameResult (SearchForGameResult(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Route
import Data.Profile
import Data.Maybe
import Capability.Game.StartGame (searchGame)

type State =
  { currentUser :: Maybe Profile
  , sid :: Maybe SubscriptionId
  }

data Action = Initialize | StartSearching

component :: forall query output m
    . MonadAff m
    => MonadStore Store.Action Store.Store m
    => StartGame m
    => H.Component query Unit output m
component =
    connect (selectEq _.currentUser) $ H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval
            { handleAction = handleAction
            , initialize = Just Initialize
            }
        }

initialState { context: currentUser } = { currentUser: currentUser, sid: Nothing }

render :: forall output m. MonadAff m => State -> H.ComponentHTML output () m
render { currentUser: currentUser } =
      HH.div_
        [ header  currentUser Game
        , HH.text "Waiting for game"
        , footer
        ]

handleAction :: forall output m. MonadAff m => StartGame m => Action -> H.HalogenM State Action () output m Unit
handleAction action = case action of
    Initialize -> do
        id <- H.subscribe =<< startSearching
        H.modify_ _ { sid = Just id }
        pure unit
    StartSearching -> searchGame >>= \result -> case result of
        GameNotFound -> trace "still searching" \_ -> pure unit
        GameFound gameId -> do
            state <- H.get
            let
               sid = state.sid
            trace "Found!!!!" \_ -> maybe (pure unit) H.unsubscribe sid
            pure unit

startSearching :: forall m a. MonadAff m => m (HS.Emitter Action)
startSearching = do
    { emitter, listener } <- H.liftEffect HS.create
    _ <- H.liftAff $ Aff.forkAff $ forever do
      H.liftEffect $ HS.notify listener StartSearching
      Aff.delay $ Milliseconds 2000.0
    pure emitter