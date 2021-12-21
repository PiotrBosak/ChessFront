module Conduit.Component.HTML.Footer where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Component.HTML.Utils (css)

footer :: forall i p. HH.HTML i p
footer =
  HH.footer_
    [ HH.div
        [ css "container" ]
        [ HH.a
            [ css "logo-font-black"
            , HP.href "/"
            ]
            [ HH.text "chess" ]
        , HH.span
            [ css "attribution" ]
            [ HH.text "Chess app" ]
        ]
    ]
