module Api.Utils where
import Affjax (request)
import Api.Request (BaseURL, RequestOptions, Token, defaultRequest, readToken, writeToken)
import Capability.LogMessages (class LogMessages, logError)
import Capability.Now (class Now)
import Data.Profile (Profile)
import Data.Username (Username)
import Store (Action(..), Store)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Prelude


mkRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> Maybe Token
  -> m (Maybe Json)
mkRequest opts token = do
    { baseUrl } <- getStore
    response <- liftAff $ request $ defaultRequest baseUrl token opts
    pure $ hush $ rmap _.body response

mkRequestNoToken
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> m (Maybe Json)
mkRequestNoToken opts = mkRequest opts Nothing


mkAuthRequest
  :: forall m
   . MonadAff m
  => MonadStore Action Store m
  => RequestOptions
  -> m (Maybe Json)
mkAuthRequest opts = do
    token <- liftEffect $ readToken
    mkRequest opts token

authenticate
  :: forall m a
   . MonadAff m
  => MonadStore Action Store m
  => LogMessages m
  => Now m
  => (BaseURL -> a -> m (Either String (Tuple Token Profile)))
  -> a
  -> m (Maybe Profile)

authenticate req fields = do
  { baseUrl } <- getStore
  req baseUrl fields >>= case _ of
    Left err -> logError err *> pure Nothing
    Right (Tuple token profile) -> do
      liftEffect do
        writeToken token
      updateStore $ LoginUser profile
      pure (Just profile)

decode :: forall m a. LogMessages m => Now m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing
decode codec (Just json) = case CA.decode codec json of
  Left err -> logError (printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)

decodeWithUser
  :: forall m a
   . MonadEffect m
  => MonadStore Action Store m
  => LogMessages m
  => Now m
  => (Maybe Username -> JsonCodec a)
  -> Maybe Json
  -> m (Maybe a)
decodeWithUser codec json = do
  { currentUser } <- getStore
  decode (codec (_.username <$> currentUser)) json
