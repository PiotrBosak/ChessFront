module Data.GameActions where

import Logic.Domain
import Data.PlayerData
import Prelude

type MakeMove =
    { playerData :: PlayerData
    , from :: Position
    , to :: Position
    }

type DrawProposal =
    { playerData :: PlayerData }
type DrawProposalAnswer =
    { decision :: DrawProposalDecision }
data DrawProposalDecision = Accept | Reject

type Fortfeit =
    { playerData :: PlayerData }

