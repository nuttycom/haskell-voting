{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Voting.Ranked.Borda where

import Control.Monad.Except
import Control.Monad.State

import Data.Foldable (maximumBy, toList)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Map as M
import qualified Data.Set as S
import Voting.Ranked.Types

data BallotError a = BallotCandidateMismatch (S.Set a) (S.Set a)
  
countVotes :: (Traversable f, Ord a, MonadError (BallotError a) m) => f (Ballot a) -> m a
countVotes ballots = do
  rankings <- evalStateT (traverse allocateRankings ballots) Nothing
  let ranked = M.toList $ M.unionsWith (+) (toList rankings)
      maxRanked = maximumBy (comparing snd) ranked
  pure $ fst maxRanked
  
allocateRankings :: (Ord a, MonadState (Maybe (S.Set a)) m, MonadError (BallotError a) m) => Ballot a -> m (M.Map a Int)
allocateRankings (Ballot ranked) = do
  candidatesMay <- get
  let ballotCandidates = S.fromList ranked
      candidates = fromMaybe ballotCandidates candidatesMay
  if candidates == ballotCandidates
    then 
      do put $ Just candidates
         pure $ M.fromList $ zip (reverse ranked) [0..]
    else 
      throwError $ BallotCandidateMismatch candidates ballotCandidates
