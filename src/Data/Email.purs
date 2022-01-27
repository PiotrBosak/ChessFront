module Data.Email where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

-- | This type exists purely as an identifier to distinguish it from a normal `String`, so we'll
-- | create a simple newtype which can be freely wrapped or unwrapped.
newtype Email = Email String

derive instance newtypeEmail :: Newtype Email _
derive instance eqEmail :: Eq Email
derive instance ordEmail :: Ord Email

codec :: JsonCodec Email
codec = wrapIso Email CA.string
