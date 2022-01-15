module Component.GameTypeChooser where

import BoardFactory
import CSS.Common
import CSS.Common
import Component.BoardComponent
import Component.HTML.Header
import Conduit.Component.HTML.Footer
import Control.Plus
import Data.Array
import Data.Array
import Data.Int
import Data.Maybe
import Data.Maybe
import Data.Newtype
import Data.Profile
import Data.Route
import Debug
import Debug
import Logic.Domain
import Logic.Domain
import Logic.Game
import Halogen.HTML.CSS
import Prelude
import Prelude
import Utils

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import CSS as C
import CSS as C
import Component.HTML.Board as B
import Component.HTML.Utils (css, maybeElem, safeHref, whenElem)
import Data.Either (hush)
import Data.Either (hush)
import Data.Foldable (elem)
import Data.List as L
import Data.List as L
import Data.Map.Internal as M
import Data.Map.Internal as M
import Data.Maybe (Maybe(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype as DN
import Data.Newtype as DN
import Data.Route as DR
import Logic.Domain as D
import Logic.Domain as D
import Effect (Effect)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Class.Console (log)
import Halogen as H
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Halogen.Store.Select (selectEq)
import Halogen.VDom.Driver (runUI)
import MapUtils as MU
import MapUtils as MU
import Routing.Duplex as RD
import Routing.Hash (getHash)
import Slug (Slug)
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.Event as Event

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
            [ HH.button [ (HE.onClick \_ -> TenMinute), css "gtc-box" ] [ HH.text " minutes aaaa" ]
            , HH.button [ HE.onClick \_ -> TwentyMinute, css "gtc-box" ] [ HH.text "20 aaa" ]
            , HH.button [ HE.onClick \_ -> FiveMinute, css "gtc-box" ] [ HH.text "5 minutes" ]
            , HH.button [ HE.onClick \_ -> OneMinute, css "gtc-box" ] [ HH.text "1 minute" ]
            ]
        , HH.div
            [ css "gtc-play-type" ]
            [ HH.button [ (HE.onClick \_ -> PlayWithFriend), css "gtc-button" ] [ HH.text "dlaczego nie dziala" ]
            , HH.br_
            , HH.br_
            , HH.button [ HE.onClick \_ -> PlayWithComputer, css "gtc-button" ] [ HH.text "aaabbcc aaa aaaatttt" ]
            , HH.br_
            , HH.br_
            , HH.button [ HE.onClick \_ -> WatchGame, css "gtc-button" ] [ HH.text "cccd ello stary" ]
            ]
        ]
    ]

handleAction :: forall m state. MonadAff m => Action -> H.HalogenM state Action () Output m Unit
handleAction _ = H.raise Clicked
