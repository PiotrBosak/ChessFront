module Data.Route where

import Prelude
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Profile
import Data.Username as Username
import Routing.Duplex (RouteDuplex', as, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))
import Slug (Slug)
import Slug as Slug

data Route
  = Home
  | Game
  | Board
  | Register
  | Login
  | WaitForGame

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route
instance showRoute :: Show Route where
  show Game = "game"
  show Home = "home"
  show Board = "board"
  show Register = "register"
  show Login = "login"
  show WaitForGame = "waitForGame"

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "Game": "game" / noArgs
  , "Board": "board" / noArgs
  , "Register": "registers" / noArgs
  , "Login": "login" / noArgs
  , "WaitForGame": "waitForGame" / noArgs
  }

slug :: RouteDuplex' String -> RouteDuplex' Slug
slug = as Slug.toString (Slug.parse >>> note "Bad slug")

uname :: RouteDuplex' String -> RouteDuplex' Username.Username
uname = as Username.toString (Username.parse >>> note "Bad username")
