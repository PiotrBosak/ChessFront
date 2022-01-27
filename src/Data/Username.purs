module Data.Username where

import Prelude

import Data.Maybe
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as CA
import Data.Newtype (class Newtype)
import Data.Profunctor (wrapIso)

newtype Username = Username String

derive instance newtypeUsername :: Newtype Username _
derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username

codec :: JsonCodec Username
codec = wrapIso Username CA.string
parse :: String -> Maybe Username
parse "" = Nothing
parse str = Just (Username str)

-- | While we don't want to be able to write or manipulate a `Username` without passing validation,
-- | we should still be able to read the string inside. Providing this function makes `Username`
-- | a read-only type.
toString :: Username -> String
toString (Username str) = str
