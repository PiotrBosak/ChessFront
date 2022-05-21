module Component.GameTypeChooser where

import Component.HTML.Board as B
import Component.HTML.Utils (css)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude (Unit, unit, ($))

data Action
  = PlayWithFriend
  | PlayWithComputer
  | TenMinute
  | TwentyMinute
  | FiveMinute
  | OneMinute
  | WatchGame

gameTypeComponent :: forall query input m. MonadAff m => H.Component query input Action m
gameTypeComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall state. state -> Unit
initialState _ = unit

render :: forall state m. MonadAff m => state -> H.ComponentHTML Action () m
render _ =
  HH.div_
    [ HH.div
        [ css "gtc-container container min-width: 550px" ]
        [ B.board
        , HH.div
            [ css "lobby gtc-grid" ]
            [ HH.button [ HE.onClick \_ -> OneMinute, css "gtc-box" ] [ HH.text "2 minute" ]
            , HH.button [ HE.onClick \_ -> FiveMinute, css "gtc-box" ] [ HH.text "5 minutes" ]
            , HH.button [ HE.onClick \_ -> TenMinute, css "gtc-box" ] [ HH.text "10 minutes" ]
            , HH.button [ HE.onClick \_ -> TwentyMinute, css "gtc-box" ] [ HH.text "20 minutes" ]
            ]
        , HH.div
            [ css "gtc-play-type" ]
            [ HH.button [ (HE.onClick \_ -> PlayWithFriend), css "gtc-button" ] [ HH.text "Play with friends" ]
            , HH.br_
            , HH.br_
            , HH.button [ HE.onClick \_ -> PlayWithComputer, css "gtc-button" ] [ HH.text "Play against computer" ]
            , HH.br_
            , HH.br_
            , HH.button [ HE.onClick \_ -> WatchGame, css "gtc-button" ] [ HH.text "Watch other games" ]
            ]
        ]
    ]

handleAction :: forall m state. MonadAff m => Action -> H.HalogenM state Action () Action m Unit
handleAction = H.raise