module Capability.Game.PollGame where

import Prelude
import Data.Route (Route)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Profunctor (dimap)
import Data.Variant as V
import Type.Proxy (Proxy(..))
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
class Monad m <= PollGame m where
    pollGame :: m PollGameResult


type GameId = String

data PollGameResult = GameFound GameId | GameNotFound

instance halogenPollGame :: PollGame m => PollGame (HalogenM st act slots msg m) where
    pollGame = lift pollGame

pollGameResultCodec :: CA.JsonCodec PollGameResult
pollGameResultCodec =
    dimap toVariant fromVariant $ CAV.variantMatch
      { gameFound: Right CA.string
      , gameNotFoundYet: Left unit
      }
    where
        toVariant = case _ of
            GameFound gameId -> V.inj (Proxy :: _ "gameFound") gameId
            GameNotFound -> V.inj (Proxy :: _ "gameNotFoundYet") unit
        fromVariant = V.match
            { gameFound: GameFound
            , gameNotFoundYet: \_ -> GameNotFound
            }