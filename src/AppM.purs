module AppM where

import Capability.Game.StartGame (class StartGame)
import Data.Maybe
import Debug
import Capability.Game.PollGame
import Capability.Now (class Now)
import Api.Endpoint
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CAR
import Capability.User (class ManageUser)
import Data.StartGameResult (StartComputerGameResult(..), StartMultiGameResult(..))
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, bind, discard, pure, unit, ($), (<<<))
import Api.Request
import Api.Utils
import Capability.LogMessages (class LogMessages)
import Capability.Navigate (class Navigate, navigate)
import Data.Log as Log
import Data.Route as Route
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT, updateStore)
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Safe.Coerce (coerce)
import Store (Action(..), LogLevel(..), Store)
import Store as Store

newtype AppM a = AppM (StoreT Store.Action Store.Store Aff a)

runAppM :: forall q i o. Store.Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadStoreAppM :: MonadStore Action Store AppM
instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    { logLevel } <- getStore
    liftEffect case logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance manageUserAppM :: ManageUser AppM where
  loginUser =
    authenticate login

  registerUser =
    authenticate register

instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.routeCodec

  logout = do
    liftEffect $ removeToken
    updateStore LogoutUser
    navigate Route.Home

instance startGameAppM :: StartGame AppM where
    startComputerGame = pure StartComputerGameResult
    startMultiGame = do
       let
         method = Post $ Nothing
       mbJson <- mkAuthRequest { endpoint: StartMultiGame, method }
       pure StartMultiGameResult

instance pollGameAppM :: PollGame AppM where
    pollGame = do
        let
            method = Post $ Nothing
        mbJson <- mkAuthRequest { endpoint: PollGame, method }
        result <- decode pollGameResultCodec mbJson
        trace "Hello, looking for game" \_ -> pure $ maybe GameNotFound (\x -> x) result