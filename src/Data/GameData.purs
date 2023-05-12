module Data.GameData where

import Prelude

newtype GameId = GameId String
type GameData =
  { id :: GameId }

