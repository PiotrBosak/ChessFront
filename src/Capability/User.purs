module Capability.User where

import Prelude
import Api.Request (LoginFields, RegisterFields)
import Data.Email (Email)
import Data.Profile (Profile, ProfileRep, ProfileWithEmail)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)


class Monad m <= ManageUser m where
  loginUser :: LoginFields -> m (Maybe Profile)
  registerUser :: RegisterFields -> m (Maybe Profile)
--  getCurrentUser :: m (Maybe ProfileWithEmail)

instance manageUserHalogenM :: ManageUser m => ManageUser (HalogenM st act slots msg m) where
    loginUser = lift <<< loginUser
    registerUser = lift <<< registerUser
--    getCurrentUser = lift getCurrentUser