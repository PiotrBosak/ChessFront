module Store where

import Prelude

import Data.Maybe (Maybe(..))
import Api.Request (BaseURL)
import Data.Profile

data LogLevel = Dev | Prod

derive instance eqLogLevl :: Eq LogLevel
derive instance orgLogLevl :: Ord LogLevel

type Store =
    -- tutaj chyba można dodać ten socket manager albo avar socket manager
  { logLevel :: LogLevel
  , baseUrl :: BaseURL
  , currentUser :: Maybe Profile
  }

data Action
  = LoginUser Profile
  | LogoutUser

-- | Finally, we'll map this action to a state update in a function called a
-- | 'reducer'. If you're curious to learn more, see the `halogen-store`
reduce :: Store -> Action -> Store
reduce store = case _ of
  LoginUser profile ->
    store { currentUser = Just profile }

  LogoutUser ->
    store { currentUser = Nothing }

