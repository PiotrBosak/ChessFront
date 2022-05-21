module Capability.Game.StartGame where

import Prelude
import Data.StartGameResult (StartComputerGameResult, StartMultiGameResult)
import Data.Route (Route)
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)

class Monad m <= StartGame m where
  startMultiGame ::  m StartMultiGameResult
  startComputerGame :: m StartComputerGameResult

instance startGameHalogen :: StartGame m => StartGame (HalogenM st act slots msg m) where
    startMultiGame = lift startMultiGame
    startComputerGame = lift startComputerGame