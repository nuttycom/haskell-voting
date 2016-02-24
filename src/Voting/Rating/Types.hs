module Voting.Rating.Types where

import Data.Map

newtype Ballot a b = Ballot { ratings :: Map a b }
