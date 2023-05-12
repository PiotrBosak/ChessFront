module Main where

import Api.Request (BaseURL(..), readToken)
import AppM (runAppM)
import Data.Maybe (Maybe(..))
import Data.Profile (Profile)
import Effect.Aff (launchAff_)
import Prelude (Unit, bind, pure, unit, void, when, ($), (/=), (>>=))
import Store (LogLevel(..), Store)
import Component.Router as Router
import Data.Route (routeCodec)
import Effect (Effect)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  let
    baseUrl = BaseURL "http://localhost:8080"
    logLevel = Dev

  currentUser :: Maybe Profile <- (liftEffect readToken) >>= case _ of
    Nothing ->
      pure Nothing
    Just _ ->
      pure Nothing

  let
    initialStore :: Store
    initialStore = { baseUrl, logLevel, currentUser }

  rootComponent <- runAppM initialStore Router.routerComponent
  halogenIO <- runUI rootComponent unit body
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) $ launchAff_ do
      _response <- halogenIO.query $ H.mkTell $ Router.Navigate new
      pure unit

