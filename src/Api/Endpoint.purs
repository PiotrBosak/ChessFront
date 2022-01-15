module Api.Endpoint where
import Prelude hiding ((/))

import Data.Route (slug, uname)
import Data.Profile (Username)
import Data.Generic.Rep (class Generic)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Routing.Duplex (RouteDuplex', int, optional, prefix, root, segment, string)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/), (?))
import Slug (Slug)

-- | First, let's define a few types necessary for our larger `Endpoint` type.

-- | Some endpoints are paginated and accept a limit (maximum count) and offset (number of items
-- | to skip over). Since some endpoints accept pagination in addition to other parameters, we'll
-- | create a row that can be shared by multiple types.
type PaginationRep =
  ( limit :: Maybe Int
  , offset :: Maybe Int
  )

type Pagination = { | PaginationRep }


type ArticleParams =
  { tag :: Maybe String
  , author :: Maybe Username
  , favorited :: Maybe Username
  | PaginationRep
  }

-- | It is annoying to have none or a single parameter and have to fill in all the others with
-- | `Nothing` values. This value pre-fills all parameters with `Nothing` so you can update only
-- | the ones you need. If the type ever gains new possible parameters, existing code written this
-- | way will continue to work.
-- |
-- | ```purescript
-- | onlyTag = noArticleParams { tag = Just "tag" }
data Endpoint
  = Login
  | User
  | Users
derive instance genericEndpoint :: Generic Endpoint _

-- Our codec will cause a compile-time error if we fail to handle any of our
-- route cases.

-- | We need to be able to write our `Endpoint` type to a valid path in order to make requests. We
-- | can use `routing-duplex` the same way we did with our `Route` type to provide both a printer
-- | (write to `String`) and a parser (parse from `String`) that stays in sync with our `Endpoint`
-- | type automatically.
-- |
-- | For a full treatment of how this function produces both a parser and printer guaranteed to
-- | produce valid paths, see the `routing-duplex` tutorial:
-- | https://github.com/natefaubion/purescript-routing-duplex/tree/v0.2.0
endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ prefix "api" $ sum
  { "Login": "users" / "login" / noArgs
  , "User": "user" / noArgs
  , "Users": "users" / noArgs
  }
