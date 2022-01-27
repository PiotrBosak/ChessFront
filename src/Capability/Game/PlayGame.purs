module Capability.Game.PlayGame where

import Data.GameActionResult
import Data.GameData
import Data.GameActions
import Data.PlayerData
import Prelude

class Monad m <= PlayGame m where
   move :: MakeMove -> GameData -> m MakeMoveResult
   proposeDraw :: DrawProposal -> GameData -> m ProposeDrawResult
   answerDrawProposal :: DrawProposalAnswer -> GameData -> m DrawProposalAnswerResult
   fortfeit :: Fortfeit -> GameData -> m FortfeitResult
