module Data.Profile where

import Prelude
import Data.Maybe (Maybe(..))

newtype Username = Username String
newtype Avatar = Avatar String

type ProfileRep row =
  ( username :: Username
  , bio :: Maybe String
  , image :: Maybe Avatar
  | row
  )

type Profile = { | ProfileRep () }




