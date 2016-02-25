{-# LANGUAGE FlexibleContexts #-}

module Voting.Approval where

import Control.Monad.Except

import Data.Foldable (maximumBy, foldl', toList)
import qualified Data.Map as M
import Data.Ord (comparing)

data BallotError a = EmptyBallotList
                   | NoWinner

newtype Ballot a = Ballot { approved :: [a] }

countVotes :: (Foldable f, Ord a, MonadError (BallotError a) m) => f (Ballot a) -> m a
countVotes ballots = 
  if null ballots
    then throwError EmptyBallotList
    else pure . fst . maximumBy (comparing snd) $ M.toList counts
  where
    counts = foldl' (flip $ M.alter (Just . maybe 1 (1 +))) M.empty (approved =<< toList ballots)

