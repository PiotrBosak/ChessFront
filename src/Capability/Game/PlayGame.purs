module Capability.Game.PlayGame where

import Prelude
import Data.GameActionResult (DrawProposalAnswerResult, FortfeitResult, MakeMoveResult, ProposeDrawResult)
import Data.GameActions (DrawProposal, DrawProposalAnswer, Fortfeit, MakeMove, ReceiveMove)
import Halogen.Subscription as HS
import Data.GameData (GameData)


class Monad m <= PlayGame m where
  move :: MakeMove -> GameData -> m MakeMoveResult
  enemyMoves :: m (HS.Emitter ReceiveMove)
  proposeDraw :: DrawProposal -> GameData -> m ProposeDrawResult
  answerDrawProposal :: DrawProposalAnswer -> GameData -> m DrawProposalAnswerResult
  forfeit :: Fortfeit -> GameData -> m FortfeitResult
