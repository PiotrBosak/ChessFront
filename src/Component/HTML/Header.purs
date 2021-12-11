-- | This module exports a pure HTML function to render a consistent header throughout the app.
module Component.HTML.Header where

import Prelude

import Component.HTML.Utils (css, maybeElem, safeHref, whenElem)
import Data.Profile
import CSS as C
import CSS.Common
import Halogen.HTML.CSS as HC
import Data.Route (Route(..))
import Data.Maybe (Maybe, isNothing, isJust)
import Data.Monoid (guard)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | Our header will be a pure render function, but we'll require a route as an argument so we can
-- | judge whether a link should display active or not. We'll allow for any profile record type so
-- | long as it has our core fields -- this makes the header reusable across pages despite which
-- | variation on `Profile` they use.
header :: forall i p r. Maybe { | ProfileRep r } -> Route -> HH.HTML i p
header currentUser route =
  HH.nav
    [ css "navbar navbar-light" ]
    [ HH.div
        [ css "container" ]
        [ HH.a
            [ css "navbar-brand"
            , safeHref Home
            ]
            [ HH.text "Chess" ]
        , HH.ul
            [ css "nav navbar-nav pull-xs-right" ]
            [ navItem Home
                [ HH.text "Home" ]
            , whenElem (isJust currentUser) \_ ->
                navItem Home
                  [ HH.i
                      [ css "ion-compose" ]
                      [ HH.text " New Post" ]
                  ]
            , whenElem (isJust currentUser) \_ ->
                navItem Home
                  [ HH.i
                      [ css "ion-gear-a" ]
                      [ HH.text " Settings" ]
                  ]
            , maybeElem currentUser \profile ->
                navItem Game
                  [ HH.img
                      [ css "user-pic"
                      , HP.src $ ""
                      ]
                  , HH.text $ ""
                  ]
            , whenElem (isNothing currentUser) \_ ->
                navItem Game
                  [ HH.text "Log in" ]
            , whenElem (isNothing currentUser) \_ ->
                navItem Game
                  [ HH.text "Sign up" ]
            ]
        ]
    ]

  where

  navItem r html =
    HH.li
      [ css "nav-item" ]
      [ HH.a
          [ css $ "nav-link" <> guard (route == r) " active"
          , safeHref r
          ]
          html
      ]