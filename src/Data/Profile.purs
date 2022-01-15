module Data.Profile where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Email
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)

newtype Username = Username String
derive instance eqUsername :: Eq Username
derive instance ordUsername :: Ord Username

codec :: JsonCodec Username
codec = dimap (\(Username user) -> user) Username CA.string

-- | This function requires a string to pass some validation before being considered a valid
-- | `Username`. For now, we'll just enforce a username is non-empty, but we might introduce more
-- | sophisticated validation later on.
parse :: String -> Maybe Username
parse "" = Nothing
parse str = Just (Username str)

-- | While we don't want to be able to write or manipulate a `Username` without passing validation,
-- | we should still be able to read the string inside. Providing this function makes `Username`
-- | a read-only type.
toString :: Username -> String
toString (Username str) = str


type ProfileRep row =
  ( username :: Username
  , bio :: Maybe String
  | row
  )

type Profile = { | ProfileRep () }

type ProfileWithEmail = { | ProfileRep (email :: Email) }

type ProfileWithEmailPassword = { | ProfileRep (email :: Email, password :: Maybe String) }
profileCodec :: JsonCodec Profile
profileCodec =
  CAR.object "Profile"
    { username: codec
    , bio: CAC.maybe CA.string
    }

