module Capability.Game.PlayGame where

import Prelude
import Data.GameActionResult (DrawProposalAnswerResult, FortfeitResult, MakeMoveResult, ProposeDrawResult)
import Data.GameActions (DrawProposal, DrawProposalAnswer, Fortfeit, MakeMove)
import Data.GameData (GameData)


class Monad m <= PlayGame m where
  move :: MakeMove -> GameData -> m MakeMoveResult
  proposeDraw :: DrawProposal -> GameData -> m ProposeDrawResult
  answerDrawProposal :: DrawProposalAnswer -> GameData -> m DrawProposalAnswerResult
  fortfeit :: Fortfeit -> GameData -> m FortfeitResult
