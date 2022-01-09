module Component.GameTypeChooser where

import Prelude
import Domain as D
import Data.Route as DR
import Affjax as AX
import Component.HTML.Header
import Conduit.Component.HTML.Footer
import Data.Newtype
import Slug (Slug)
import Halogen.Store.Connect (Connected, connect)
import Utils
import Data.Newtype as DN
import Halogen.HTML.CSS
import Halogen.Store.Select (selectEq)
import Affjax.ResponseFormat as AXRF
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event)
import Effect.Class.Console (log)
import Web.Event.Event as Event
import BoardFactory
import MapUtils as MU
import Data.Map.Internal as M
import Data.List as L
import Data.Maybe
import Domain
import CSS as C
import CSS.Common
import Halogen.HTML.CSS as HC
import Data.Array
import Game
import Control.Plus
import Data.Int
import Prelude
import Debug
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen.HTML.Properties as HP
import Domain
import Domain as D
import Data.Newtype as DN
import Data.Array
import CSS as C
import CSS.Common
import Halogen.HTML.CSS as HC
import Data.List as L
import Data.Map.Internal as M
import Game
import MapUtils as MU
import Debug
import Data.Either (hush)
import Halogen.HTML.Events as HE
import Data.Foldable (elem)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Component.BoardComponent
import Halogen.Store.Select (selectEq)
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Type.Proxy (Proxy(..))
import Data.Route
import Data.Profile
import Data.Maybe
import Component.HTML.Board as B
import Component.HTML.Utils (css, maybeElem, safeHref, whenElem)

data Action
  = PlayWithFriend
  | PlayWithComputer
  | TenMinute
  | TwentyMinute
  | FiveMinute
  | OneMinute
  | WatchGame

data Output = Clicked

gameTypeComponent :: forall query input m. MonadAff m => H.Component query input Output m
gameTypeComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState _ = unit

render :: forall state m. MonadAff m => state -> H.ComponentHTML Action () m
render _ =
  HH.div_
    [ HH.div
        [ css "gtc-container container" ]
        [ B.board
        , HH.div
            [ css "lobby gtc-grid" ]
            [ HH.button [ (HE.onClick \_ -> TenMinute), css "gtc-box" ] [ HH.text "90 minutes" ]
            , HH.button [ HE.onClick \_ -> TwentyMinute, css "gtc-box" ] [ HH.text "20 minutes" ]
            , HH.button [ HE.onClick \_ -> FiveMinute, css "gtc-box" ] [ HH.text "5 minutes" ]
            , HH.button [ HE.onClick \_ -> OneMinute, css "gtc-box" ] [ HH.text "1 minute" ]
            ]
        , HH.div
            [ css "gtc-play-type" ]
            [ HH.button [ (HE.onClick \_ -> PlayWithFriend), css "gtc-button" ] [ HH.text "Play with friend" ]
            , HH.br_
            , HH.br_
            , HH.button [ HE.onClick \_ -> PlayWithComputer, css "gtc-button" ] [ HH.text "Play with Computer" ]
            , HH.br_
            , HH.br_
            , HH.button [ HE.onClick \_ -> WatchGame, css "gtc-button" ] [ HH.text "Watch games" ]
            ]
        ]

    ]

handleAction :: forall m state. MonadAff m => Action -> H.HalogenM state Action () Output m Unit
handleAction _ = H.raise Clicked
