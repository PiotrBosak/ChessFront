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

header :: forall i p r. Maybe { | ProfileRep r } -> Route -> HH.HTML i p
header currentUser route =
  HH.nav
    [ css "navbar navbar-light" ]
    [ HH.div
        [ css "container" ]
        [ HH.a
            [ css "navbar-brand-black"
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
                navItem Home
                  [ HH.img
                      [ css "user-pic"
                      , HP.src $ ""
                      ]
                  , HH.text $ ""
                  ]
            , whenElem (isNothing currentUser) \_ ->
                navItem Login
                  [ HH.text "Log in" ]
            , whenElem (isNothing currentUser) \_ ->
                navItem Register
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