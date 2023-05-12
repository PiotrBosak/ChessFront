module Capability.Game.StartGame where

import Prelude
import Data.StartGameResult (StartComputerGameResult, StartMultiGameResult, SearchForGameResult)
import Data.Route (Route)
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import Effect.AVar

class Monad m <= StartGame m where
  startMultiGame ::  m StartMultiGameResult
  searchGame :: m  SearchForGameResult
  startComputerGame :: m StartComputerGameResult

instance startGameHalogen :: StartGame m => StartGame (HalogenM st act slots msg m) where
    startMultiGame = lift startMultiGame
    searchGame = lift searchGame
    startComputerGame = lift startComputerGame