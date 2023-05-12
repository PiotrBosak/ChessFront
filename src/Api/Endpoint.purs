module Api.Endpoint where

import Prelude hiding ((/))

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Username (Username)
import Routing.Duplex (RouteDuplex', prefix, root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

type PaginationRep =
  ( limit :: Maybe Int
  , offset :: Maybe Int
  )

type Pagination = { | PaginationRep }

data Endpoint
  = Login
  | User
  | Users
  | StartMultiGame
  | PollGame

derive instance genericEndpoint :: Generic Endpoint _

endpointCodec :: RouteDuplex' Endpoint

endpointCodec = root $ prefix "v1" $ sum
  { "Login": "auth" / "login" / noArgs
  , "User": "user" / noArgs
  , "Users": "auth" / "users" / noArgs
  , "StartMultiGame": "gameSearch" / "start" / noArgs
  , "PollGame": "gameSearch" / "poll" / noArgs
  }
