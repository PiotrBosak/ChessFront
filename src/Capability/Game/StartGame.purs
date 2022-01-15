module Capability.Game.StartGame where

import Data.Profile
import Data.StartGameResult
import Prelude

class Monad m <= StartGame m where
    startMultiGame :: ProfileWithEmailPassword -> m StartMultiGameResult
    startComputerGame :: ProfileWithEmailPassword -> m StartComputerGameResult


