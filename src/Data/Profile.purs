module Data.Profile where

import Prelude

import Data.Codec.Argonaut (JsonCodec)
import Data.Email
import Data.Username (codec,Username)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Maybe (Maybe(..))
import Data.Profunctor (dimap)


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
