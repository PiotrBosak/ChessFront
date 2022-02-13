module AppM where

import Prelude
import Api.Endpoint (Endpoint(..))
import Api.Request (RequestMethod(..))
import Data.Log as Log
import Api.Request as Request
import Capability.User
import Capability.Now
import Api.Utils (authenticate, decode, decodeWithUser, mkAuthRequest, mkRequest)
import Capability.LogMessages (class LogMessages)
import Capability.Navigate (class Navigate, navigate)
import Capability.Now (class Now)
import Data.Profile as Profile
import Data.Route as Route
import Store (Action(..), LogLevel(..), Store)
import Store as Store
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
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
    authenticate Request.login

  registerUser =
    authenticate Request.register

instance navigateAppM :: Navigate AppM where
  navigate =
    liftEffect <<< setHash <<< print Route.routeCodec

  logout = do
    liftEffect $ Request.removeToken
    updateStore LogoutUser
    navigate Route.Home

--  getCurrentUser = do
--    mbJson <- mkAuthRequest { endpoint: User, method: Get }
--    map (map _.user)
--      $ decode (CAR.object "User" { user: Profile.profileWithEmailCodec }) mbJson-    getCurrentUser :: m (Maybe ProfileWithEmail)