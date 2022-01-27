module Api.Request
  ( Token -- constructor and decoders not exported
  , BaseURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  , RegisterFields(..)
  , LoginFields(..)
  , login
  , register
  , readToken
  , writeToken
  , removeToken
  ) where

import Prelude

import Affjax (Request, printError, request)
import Affjax.RequestBody as RB
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Api.Endpoint (Endpoint(..), endpointCodec)
import Data.Email (Email)
import Data.Email as Email
import Data.Profile (Profile)
import Data.Profile as Profile
import Data.Username
import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec as Codec
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Routing.Duplex (print)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)

newtype Token = Token String

derive instance eqToken :: Eq Token

derive instance ordToken :: Ord Token

instance showToken :: Show Token where
  show (Token _) = "Token {- token -}"

newtype BaseURL = BaseURL String
data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put (Maybe Json)
  | Delete

type RequestOptions =
  { endpoint :: Endpoint
  , method :: RequestMethod
  }

defaultRequest :: BaseURL -> Maybe Token -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) auth { endpoint, method } =
  { method: Left requestMethod
  , url: baseUrl <> print endpointCodec endpoint
  , headers: case auth of
      Nothing -> []
      Just (Token t) -> [ RequestHeader "Authorization" $ "Token " <> t ]
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , timeout: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple requestMethod body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing

type RegisterFields =
  { email :: Email
  , password :: String
  , username :: Username
  }

registerCodec :: JsonCodec RegisterFields
registerCodec =
  CAR.object "RegisterFields"
    { email: Email.codec
    , password: CA.string
    , username: codec
    }

type LoginFields =
  { email :: Email
  , password :: String
  }

loginCodec :: JsonCodec LoginFields
loginCodec =
  CAR.object "LoginFields"
    { email: Email.codec
    , password: CA.string
    }

-- | This function logs a user in (if they exist), returning an auth token and the user's
-- | minimal profile.
login :: forall m. MonadAff m => BaseURL -> LoginFields -> m (Either String (Tuple Token Profile))
login baseUrl fields =
  let
    method = Post $ Just $ Codec.encode (CAR.object "User" { user: loginCodec }) { user: fields }
  in
    requestUser baseUrl { endpoint: Login, method }

-- | This function registers a user (if they don't already exist), returning an auth token and the
-- | user's minimal profile.
register :: forall m. MonadAff m => BaseURL -> RegisterFields -> m (Either String (Tuple Token Profile))
register baseUrl fields =
  let
    method = Post $ Just $ Codec.encode (CAR.object "User" { user: registerCodec }) { user: fields }
  in
    requestUser baseUrl { endpoint: Users, method }

-- | The login and registration requests share the same underlying implementation, just a different
-- | endpoint. This function can be re-used by both requests.
requestUser :: forall m. MonadAff m => BaseURL -> RequestOptions -> m (Either String (Tuple Token Profile))
requestUser baseUrl opts = do
  res <- liftAff $ request $ defaultRequest baseUrl Nothing opts
  case res of
    Left e -> pure $ Left $ printError e
    Right v -> pure $ lmap printJsonDecodeError $ decodeAuthProfile =<< Codec.decode (CAR.object "User" { user: CA.json }) v.body

-- | This JSON decoder is defined in this module because it manipulates a token. First, we'll decode
-- | only the token field from the payload, and then we'll decode everything else separately into
-- | the user's profile.
decodeAuthProfile :: { user :: Json } -> Either JsonDecodeError (Tuple Token Profile)
decodeAuthProfile { user } = do
  { token } <- Codec.decode (CAR.object "Token" { token: tokenCodec }) user
  profile <- Codec.decode Profile.profileCodec user
  pure (Tuple token profile)
  where
  tokenCodec =
    CA.prismaticCodec "Token (inner)" (Just <<< Token) (\(Token t) -> t) CA.string

-- | The following functions deal with writing, reading, and deleting tokens in local storage at a
-- | particular key. They'll be used as part of our production monad, `Conduit.AppM`.

tokenKey = "token" :: String

readToken :: Effect (Maybe Token)
readToken = do
  str <- getItem tokenKey =<< localStorage =<< window
  pure $ map Token str

writeToken :: Token -> Effect Unit
writeToken (Token str) =
  setItem tokenKey str =<< localStorage =<< window

removeToken :: Effect Unit
removeToken =
  removeItem tokenKey =<< localStorage =<< window