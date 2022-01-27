module Form.Validation where

import Prelude

import Data.Email (Email(..))
import Data.Username (Username)
import Data.Username as Username
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String as String
import Formless as F

data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidEmail
  | InvalidUsername

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"
  InvalidEmail -> "Invalid email address"
  InvalidUsername -> "Invalid username"

required :: forall form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ cond (_ /= mempty) Required

minLength :: forall form m. Monad m => Int -> F.Validation form m FormError String String
minLength min = F.hoistFnE_ $ cond (\s -> String.length s >= min) TooShort

maxLength :: forall form m. Monad m => Int -> F.Validation form m FormError String String
maxLength n = F.hoistFnE_ $ cond (\str -> String.length str <= n) TooLong

emailFormat :: forall form m. Monad m => F.Validation form m FormError String Email
emailFormat = F.hoistFnE_ $ map Email <<< cond (String.contains (String.Pattern "@")) InvalidEmail

usernameFormat :: forall form m. Monad m => F.Validation form m FormError String Username
usernameFormat = F.hoistFnE_ $ note InvalidUsername <<< Username.parse

cond :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
cond f err a = if f a then pure a else Left err

toOptional
  :: forall form m a b
   . Monoid a
  => Eq a
  => Monad m
  => F.Validation form m FormError a b
  -> F.Validation form m FormError a (Maybe b)
toOptional v = F.Validation \form val ->
  case val == mempty of
    true -> pure (pure Nothing)
    false -> (map <<< map) Just (F.runValidation v form val)
