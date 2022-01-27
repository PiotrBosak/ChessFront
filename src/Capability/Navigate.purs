module Capability.Navigate where

import Data.Route
import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import Prelude

class Monad m <= Navigate m where
    navigate :: Route -> m Unit
    logout :: m Unit


instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate
  logout = lift logout

