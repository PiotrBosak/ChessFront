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
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Route
import Capability.Game.PollGame
import Data.Profile
import Data.Maybe

type State =
  { currentUser :: Maybe Profile
  , sid :: Maybe SubscriptionId
  }

data Action = Initialize | PollGame

component :: forall query output m
    . MonadAff m
    => MonadStore Store.Action Store.Store m
    => PollGame m
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

handleAction :: forall output m. MonadAff m => PollGame m => Action -> H.HalogenM State Action () output m Unit
handleAction action = case action of
    Initialize -> do
        id <- H.subscribe =<< startPolling
        H.modify_ _ { sid = Just id }
        pure unit
    PollGame -> pollGame >>= \result -> case result of
        GameNotFound -> trace "still searching" \_ -> pure unit
        GameFound gameId -> do
            state <- H.get
            let
               sid = state.sid
            trace "Found!!!!" \_ -> maybe (pure unit) H.unsubscribe sid
            pure unit

startPolling :: forall m a. MonadAff m => m (HS.Emitter Action)
startPolling = do
    { emitter, listener } <- H.liftEffect HS.create
    _ <- H.liftAff $ Aff.forkAff $ forever do
      H.liftEffect $ HS.notify listener PollGame
      Aff.delay $ Milliseconds 2000.0
    pure emitter