module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Halogen (liftEffect)
import Halogen as H
import Affjax (request, printError)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Component.BoardComponent
import Api.Request
import Data.Profile
import Data.Route (routeCodec)
import Effect.Aff
import Data.Maybe
import AppM
import Component.Router as Router
import Store

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody

  let
    baseUrl = BaseURL "localhost:8080"
    logLevel = Dev

  currentUser :: Maybe Profile <- (liftEffect readToken) >>= case _ of
    Nothing ->
      pure Nothing
    Just token ->
      pure Nothing

  let
    initialStore :: Store
    initialStore = { baseUrl, logLevel, currentUser }

  rootComponent <- runAppM initialStore Router.routerComponent
  halogenIO <-  runUI rootComponent unit body
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
      when (old /= Just new) do
        launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new

