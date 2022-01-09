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
import Effect.Aff (launchAff_)
import Data.Maybe
import Component.Router
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

  halogenIO <- runUI routerComponent unit body
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
      when (old /= Just new) do
        launchAff_ $ halogenIO.query $ H.mkTell $ Navigate new

